// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.applications

import java.net.URI

import scalaz.zio.{IO, Schedule}

import scala.util.Try
import scalaz._
import Scalaz._

import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.console.putStrLn

object exercises extends App {

  // Web crawler app
  /**
    * + start with a set of seed URLs
    * - for each URL:
    *   + retrieve the HTML content of the URL
    *     + handle failures gracefully
    *     + retry using crazy retry strategy
    *   + parse the HTML content
    *   + feed the content and the url to the processor
    *   - identify all href links
    *   - filter href links according to some criteria
    *   - update the working set of URLs to include the href links
    *   - continue the process as long as we have URLs to crawl
    */

  final case class Url private(url: String) {
    def relative(page: String): Option[Url] = {
      Url(url + "/" + page)
    }
  }

  object Url {
    def apply(url: String): Option[Url] = Try(URI.create(url).parseServerAuthority()).toOption.map(u => new Url(url))

  }

  def getURL(url: Url): IO[Exception, String] =
    IO.syncException(scala.io.Source.fromURL(url.url)(scala.io.Codec.UTF8).mkString)

  def extractURLS(url: Url, html: String): List[Url] = {
    val pattern = "href=\"([^\"]+)\"".r

    // the reason to use Try - is to convert something that is throwing exception to value
    scala.util.Try({
      val matches = (for {m <- pattern.findAllMatchIn(html)} yield m.group(1)).toList
      for {
        m <- matches
        url <- (Url(m) orElse url.relative(m)).toList
      } yield url

    }).fold[List[Url]](_ => Nil, identity)
  }

  // Note: use constraints on methods that uses your data structures

  final case class ProcessorError[E](error: E, url: Url, html: String)

  // it's bifunctor, it's have two values and you can map on them
  final case class Crawl[E, A](error: E, value: A) {
    def leftMap[E2](f: E => E2): Crawl[E2, A] = ???
    def map[A2](f: A => A2): Crawl[E, A2] = ???
  }

  object Crawl {
    implicit def CrawlMonoid[E: Monoid, A: Monoid]: Monoid[Crawl[E, A]] = new Monoid[Crawl[E, A]] {
      override def zero: Crawl[E, A] = Crawl(mzero[E], mzero[A])
      override def append(f1: Crawl[E, A], f2: => Crawl[E, A]): Crawl[E, A] =
        Crawl(f1.error |+| f2.error, f1.value |+| f2.value)
    }

  }

  val DefaultCrawlSchedule = Schedule.once
//    (Schedule.exponential(10.milliseconds).jittered && Schedule.recurs(20)).void

  // A - processor result
  // we could have return type IO[Exception, List[A]]  but it means that we force user to use this structure
  def crawl[E: Monoid, A: Monoid](
    seed: Set[Url],
    router: Url => Set[Url],
    processor: (Url, String) => IO[E, A],
    schedule: Schedule[Exception, Unit] = DefaultCrawlSchedule
  ): IO[Exception, Crawl[E, A]] = {

    def processor1(url: Url, html: String): IO[Nothing, Crawl[E, A]] =
      processor(url, html).redeemPure(e => Crawl(e, mzero[A]), a => Crawl(mzero[E], a))

    IO.traverse(seed) { url =>
      for {
        html <- getURL(url).retry(schedule)
        crawl <- processor1(url, html)
        links <- IO.now(extractURLS(url, html).toSet.flatMap(router))
      } yield (crawl, links)
    }.map(_.foldMap()).flatMap {
      case (crawl0, links) =>
        crawl(links, router, processor).map(crawl1 => crawl0 |+| crawl1)
    }

  }

  def crawlE[E, A: Monoid](
    seeds: Set[Url],
    router: Url => Set[Url],
    processor: (Url, String) => IO[E, A]): IO[Exception, Crawl[List[ProcessorError[E]], A]] =
    crawl(seeds, router, (url, html) =>
      processor(url, html).redeem(
        e => IO.fail(List(ProcessorError(e, url, html))),
        e => IO.now(e)
      ))

  def toUrl(s: String): IO[Exception, Url] =
    IO.fromOption(Url(s)).leftMap(_ => new Exception(s"Invalid $s"))

  // tests in run

  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      urls <- IO.traverse(args)(toUrl).map(_.toSet)
      router = (url: Url) => if (url.url.contains("zio")) Set(url) else Set.empty[Url]
      processor = (url: Url, html: String) => console.putStrLn(s"Traversing $url")
      crawl <- crawlE(urls, router, processor)
      _ <- putStrLn(crawl.error.mkString("\n"))
    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))

}