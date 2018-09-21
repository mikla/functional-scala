// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.arts

import scalaz.{Functor, Monad}
import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.IO
import scalaz.zio.interop.scalaz72._
import scalaz._
import Scalaz._
import net.degoes.arts.fix.notfixed.Json.Null

import scala.concurrent.Future
import scala.reflect.ClassTag

object exercises {

  trait Console[F[_]] {
    def putStrLn(line: String): F[Unit]
    def getStrLn: F[String]
  }

  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
    implicit val ConsoleIO: Console[IO[Throwable, ?]] =
      new Console[IO[Throwable, ?]] {
        override def putStrLn(line: String): IO[Throwable, Unit] = ???
        override def getStrLn: IO[Throwable, String] = ???
      }

    def putStrLn[F[_] : Console](line: String): F[Unit] = Console[F].putStrLn(line)
    def getStrLn[F[_] : Console]: F[String] = Console[F].getStrLn
  }

  def myApp[F[_] : Console : Monad]: F[Unit] =
    for {
      _ <- Console.putStrLn("Hello")
      n <- Console.getStrLn
      _ <- Console.putStrLn("Good " + n)
    } yield ()

  // Let's say we need to add logging
  // You can go FT approach and have trait Logging and require in myApp[F[_]: Logging]
  // Or we can go with monad transformer approach
  // So we want to use Logging effect locally.

  case class LoggingT[F[_], A](run: List[String] => F[(List[String], A)]) {
    self =>
    def map[B](f: A => B)(implicit F: Functor[F]): LoggingT[F, B] =
      LoggingT(l => self.run(l).map(t => (t._1, f(t._2))))

    def flatMap[B](f: A => LoggingT[F, B])(implicit F: Monad[F]): LoggingT[F, B] =
      LoggingT[F, B](log => self.run(log).flatMap(t => f(t._2).run(t._1)))

    def eval(implicit F: Functor[F]): F[(List[String], A)] = ???
  }

  object LoggingT {
    def point[F[_] : Applicative, A](a: => A): LoggingT[F, A] =
      exercises.LoggingT(log => Applicative[F].point((log, a)))

    def log[F[_] : Applicative](line: String): LoggingT[F, Unit] =
      LoggingT(log => Applicative[F].point((line :: log, ())))

    // just deligate point and flatMap
    implicit def LoggintTMonad[F[_] : Monad]: Monad[LoggingT[F, ?]] = ???

    // it's effect of logging to any F
    def lift[F[_], A](fa: F[A])(implicit F: Functor[F]): LoggingT[F, A] = ???

  }

  def getName[F[_] : Console : Monad]: F[Unit] =
    (for {
      _ <- LoggingT.lift[F, Unit](Console.putStrLn("Hello"))
      _ <- LoggingT.log[F]("Good line")
      _ <- LoggingT.lift(Console.putStrLn("Hello"))
      n <- LoggingT.lift(Console.getStrLn)
      _ <- LoggingT.lift(Console.putStrLn("Good " + n))
    } yield (n)).eval.flatMap {
      case (log, name) =>
        Console.putStrLn(s"$name") *> Monad[F].point(name)
    }

  // This LoggerT[F, A] is just WriterT[F, List[String, A]

  // Best practice is to delete all this stuff LoggingT and introduce Logging effect in signature.

  // managing state (somebody asked specific question)
  /*
  type Task[A] = IO[Throwable, A]
  class MyIO[A](val io: Task[A]) extends AnyVal
  def createMonadState[S]: Task[MonadState[Task, S]] = ???
  def myLocalState[F[_]: MonadState[?, MyStateType]]: F[Boolean] = ???
  for {
    monadState <- createMonadState[MyStateType]
    resutl <- myLocalState[MyIO](monadState).run
  } yield result
  */

  // Q: How do we deal with structures like Future[Either[E, A]]

  def geoAPI(url: String): Future[Either[Throwable, String]] = ???
  def cacheAPI(url: Array[Byte]): Future[Either[Throwable, String]] = ???
  def queryDatabase(query: String): Future[Either[Throwable, String]] = ???

  // First: we create type synonym.

  type Effect[E, A] = Future[Either[E, Option[A]]]

  // and then

  def geoAPI_[E](url: String): Effect[E, String] = ???
  def cacheAPI_[E](url: Array[Byte]): Effect[E, String] = ???
  def queryDatabase_[E](query: String): Effect[E, String] = ???

  /**
    * Some thoughts on some type:
    * E + Option[A]
    * E + (Unit + A)
    * (E + Unit) + A
    *
    * Either[Option[E], A]
    *
    */

  // We can create Monad instance for our Effect. But it will be again monad transformers stack.
  // we could write something like this
  // case class Effect[E, A](run: Future[Either[E, Option[A]]])
  // and implement map and flatMap

  // we also could have
  // type Effect[E, A] = OptionT[EitherT[Future, E, ?], ?]

  // performance everhead.
  // we have to resort for FT.
  // we want use IO.

  trait Effect_[F[_, _]] {
    //    def monadError[E]: MonadError[F[E, ?], E]
    def monad[E]: Monad[F[E, ?]]
    def fail[E, A](e: E): F[E, A]
    def attempt[E, A](fea: F[E, A]): F[Nothing, Either[E, A]]

    def none[E, A]: F[E, A]
    def some[E, A](a: A): F[E, A]
    def fromFuture[E, A](f: Future[A]): F[E, A]
  }

  type MyIO[E, A] = IO[Option[E], A]

  object Effect {
    implicit val EffectIO: Effect_[MyIO] = new Effect_[MyIO] {
      override def monad[E]: Monad[MyIO[E, ?]] = ???
      override def fail[E, A](e: E): MyIO[E, A] = ???
      override def attempt[E, A](fea: MyIO[E, A]): MyIO[Nothing, Either[E, A]] = ???
      override def none[E, A]: MyIO[E, A] = ???
      override def some[E, A](a: A): MyIO[E, A] = ???
      override def fromFuture[E, A](f: Future[A]): MyIO[E, A] = ???
    }
  }

  // what is we need cats and monix
  // it will be just 2 times slow :D:D
  // Legacy effect monad

  type Task[A] = Future[A]
  type MyTask[E, A] = EitherT[Task, E, A]
  type MyTerribleTask[E, A] = Task[E]

  case class MyError[E: ClassTag](error: E) extends Throwable

  implicit def EffectTask[E0: ClassTag]: Effect_[MyTerribleTask] = ???

}

object hoas {

  // let's make a programming language
  // that allows us to do while loop:
  //
  // var i = 10
  // while (i < 10) { i = i + 1 }
  //

  sealed trait Expr[F[_]] {
    def IntLit(value: Int): F[Int]
    def Add(i: F[Int], r: F[Int]): F[Int]
    def Let[A, B](name: Symbol, initialValue: F[A], insideBody: F[A] => F[B]): F[B]
    def Value[A](name: Symbol): F[A]
    def UpdateVariable[A](name: Symbol, value: F[A]): F[A]
    def LessThan(left: F[Int], right: F[Int]): F[Boolean]
    def While[A](condition: F[Boolean], body: F[A]): F[Unit]
  }

  object Expr {
    def apply[F[_]](implicit F: Expr[F]): Expr[F] = F
  }

  implicit class ExprSyntax[F[_]](l: F[Int]) {
    def let[A, B](name: Symbol, value: F[A], body: F[A] => F[B])(implicit F: Expr[F]): F[B] = F.Let(name, value, body)
    def +(r: F[Int])(implicit F: Expr[F]): F[Int] = F.Add(l, r)
    // ... more operations

  }

  // here program defenition goes
  def program[F[_]: Expr]: F[Unit] = ???

//  val program: Expr[Unit] =
//    Let('i, IntLit(0),
//      While(LessThan(Value('i), IntLit(10)),
//        UpdateVariable('i, Add(Value('i), IntLit(1)))
//      )
//    )

  // problem is that it's poorely typed and I don't have to use variable that I defined in it.
  // let's try to write interpreter for it.

  case class InterpreterState(state: Map[Symbol, Any]) {
    def addValue(name: Symbol, v: Any): InterpreterState = copy(state = state + (name -> v))
    def removeVariable(name: Symbol): InterpreterState = copy(state = state - name)
  }

  def interpreter(ref: Ref[InterpreterState]): Expr[IO[String, ?]] = new Expr[IO[String, ?]] {
    override def IntLit(value: Int): IO[String, Int] = IO.now(value)
    override def Add(left: IO[String, Int], right: IO[String, Int]): IO[String, Int] =
      left.seqWith(right)(_ + _)
    override def Value[A](name: Symbol): IO[String, A] = ???
    override def UpdateVariable[A](name: Symbol, value: IO[String, A]): IO[String, A] = ???
    override def LessThan(left: IO[String, Int], right: IO[String, Int]): IO[String, Boolean] = ???
    override def While[A](condition: IO[String, Boolean], body: IO[String, A]): IO[String, Unit] = ???
    override def Let[A, B](name: Symbol, initialValue: IO[String, A], insideBody: IO[String, A] => IO[String, B]): IO[String, B] = ???
  }

  type Interpreter[A] = EitherT[State[InterpreterState, ?], String, A]

  /*
  def interpret[A0](expr: Expr[A0], ref: Ref[InterpreterState]): IO[String, A0] = {
    def interpret0[A](expr: Expr[A]): IO[String, A] =
      expr match {
        case IntLit(value) => IO.now(value)
        case Add(left, right) => interpret0(left).seqWith(interpret0(right))(_ + _)
        case Let(name, value, body) =>
          for {
            v <- interpret0(value)
            _ <- ref.update(_.addValue(name, v))
            b <- interpret0(body)
            _ <- ref.update(_.removeVariable(name))
          } yield b

        case Value(name: Symbol) => for {
          s <- ref.get
          v <- IO.fromOption(s.state.get(name)).leftMap(_ => "Unreferenced variable: " + name)
        } yield v.asInstanceOf[A]

        case UpdateVariable(name: Symbol, value: Expr[A]) =>
          for {
            v <- interpret0(value)
            p <- ref.update(_.addValue(name, v))
          } yield v

        case LessThan(left: Expr[Int], right: Expr[Int]) =>
          interpret0(left).seqWith(interpret0(right))(_ < _)

        case While(condition: Expr[Boolean], body: Expr[A]) => (for {
          b <- interpret0(condition)
          _ <- if (b) interpret0(body) else IO.unit
        } yield b).repeat(Schedule.doWhile[Boolean](identity)).void

        case _ => IO.fail("Error")
      }

    interpret0[A0](expr)
  }

  */

  // Problem is ugly cast.

}

object monad_select {

  sealed trait Parser[+E, +A] { self =>
    def map[B](f: A=> B): Parser[E, B] = ???
    def or[E1 >: E, B](that: Parser[E1, B]): Parser[E, Either[A, B]] = ???
  }


  case class Fail[E](error: E) extends Parser[E, Nothing]
  case class Succeed[A](value: A) extends Parser[Nothing, A]
  case class Character[E](error: E) extends Parser[E, Char]
  case class Repeat[E, A, B](value: Parser[E, A]) extends Parser[E, B]
  case class Alternative[E, A, B](left: Parser[E, A], right: Parser[E, B]) extends Parser[E, Either[A, B]]
  case class Literal[E](error: E, value: String) extends Parser[E, String]
  case class Zip[E, A, B](left: Parser[E, A], right: Parser[E, B]) extends Parser[E, (A, B)]
  case class Map[E, A0, A](value: Parser[E, A0], f: A0 => A) extends Parser[E, A]

  implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] = new Applicative[Parser[E, ?]] {
    override def point[A](a: => A): Parser[E, A] = ???
    override def ap[A, B](fa: =>Parser[E, A])(f: =>Parser[E, A => B]): Parser[E, B] =
      Map[E, (A => B, A), B](Zip(f, fa), t => t._1(t._2))
  }

  // (Parser.char("") |@| Parser.)

  // write compiler

  def compiler[E, A](parser: Parser[E, A]): String => Either[E, A] = {
    // we can choose representation here


    ???

  }

  // selectable functors allowed you to make decisions
  // in applicative world
  // make you applicative functor context sensitive
  // how it differs from Monad?
  // Monad can produce infinite variations
  // Select functor finite number of things, runtime decisions based on value and finite number of variations.


}


object fix {

  object notfixed {

    sealed trait Json {
      json =>
      def fold[Z](z: Z)(f: (Json, Z) => Z): Z = json match {
        case j@Null => f(j, z)
        //      case j @ Obj =>
      }
    }

    object Json {
      case object Null extends Json
      case class Obj(obj: Map[String, Json]) extends Json
    }

    def collectFieldNames(json: Json): List[String] = json.fold[List[String]](Nil) {
      case (Json.Obj(o), l) => o.keys.toList ++ l
      case (_, l) => l
    }
  }

  // so to not to write recursion by youself

  object fixpoint {
    sealed trait Json[+A]

    // this is not recursive yet.
    object Json {
      case object Null extends Json[Nothing]
      case class Obj[A](obj: Map[String, A]) extends Json[A]
      case class Num[A](value: Int) extends Json[A]
    }

    // we can regain the ability to do recursion with fix point function.
//    Json[List[String]]

    case class Fix[F[_]](unfix: F[Fix[F]])
    def fix[F[_]](f: F[Fix[F]]): Fix[F] = Fix(f)

    type JsonR = Fix[Json]

    val example: JsonR = fix(Json.Obj(
      Map("age" -> fix(Json.Num[JsonR](24)))
    ))

    // Advantage?
    // I can write fold that works with any fix structure.

    implicit val TraverseJson: Traverse[Json] = ???

    def cata[F[_]: Foldable, Z](fix: Fix[F])(algebra: F[Z] => Z): Z = ???

    // you can compose adts with fix

    case class JDate[A](time: java.time.LocalDate)
    type JsonWithDate = Fix[Coproduct[Json, JDate, ?]]
    case class Coproduct[F[_], G[_], A](run: Either[F[A], G[A]])

  }

}