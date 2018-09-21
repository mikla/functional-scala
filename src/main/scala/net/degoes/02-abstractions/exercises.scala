// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.abstractions

import java.time.LocalDate

import scalaz._
import Scalaz._

import scala.language.higherKinds

object algebra {

  /**
    * Associativity:
    * append(x, append(y, z)) === append(append(x, y), z)
    * x |+| (y |+| z) === (x |+| y) |+| z
    *
    */

  // Commeted because it's confliciting with existing. from scalaz.
  //trait Semigroup[A] {
  //  def append(l: => A, r: => A): A
  //}

  // implicit class SemigroupSyntax[A](l: A) {
  //    def |+|(r: A)(implicit S: Semigroup[A]): A = S.append(l, r)
  // }

  /*
    For Int, how else we can define Semigroup for Int?
    sum
    mutiplication
    return constant
    always return left
    always return right
    min
    max
  */

  // (A, A) => A

  //
  // EXERCISE 1
  //
  // Define a semigroup for `NotEmpty` below.
  //
  case class NotEmpty[+A](head: A, tail: Option[NotEmpty[A]])

  implicit def NotEmptySemigroup[A]: Semigroup[NotEmpty[A]] = new Semigroup[NotEmpty[A]] {
    def append(l: NotEmpty[A], r: => NotEmpty[A]): NotEmpty[A] = NotEmpty(
      l.head, l.tail match {
        case None => Some(r)
        case Some(l) => Some(append(l, r))
      }
    )
  }
  val example1 = NotEmpty(1, None) |+| NotEmpty(2, None)

  /*
    Laws:
      - append(mzero, a) === a
      - append(mzero, a) === a

    trait Monoid[A] {
      def mzero: A
    }

    What is mzero for maximum operation? - MinValue
    whar is mzer for minimum operation? - MaxValue
  */

  //
  // EXERCISE 2
  //
  // Design a permission system for securing some resource, together with a
  // monoid for the permission data structure.
  //
  sealed trait Capability

  case object Read extends Capability

  case object Write extends Capability

  type Email = String

  case class Resource(value: String)

  case class Permission(value: Map[Email, Map[Resource, Set[Capability]]] = Map.empty)

  // todo implement
  implicit val MonoidPermission: Monoid[Permission] = ???
  implicit val SemigroupPermission: Semigroup[Permission] = ???

  val example2 = mzero[Permission] |+| Permission()

  //
  // EXERCISE 3
  //
  // Define an instance of `Semigroup` for `(A, B)` when both `A` and
  // `B` form semigroups.
  //
  implicit def SemigroupTuple2[A: Semigroup, B: Semigroup]:
  Semigroup[(A, B)] = new Semigroup[(A, B)] {
    def append(l: (A, B), r: => (A, B)): (A, B) = (l._1 |+| r._1, l._2 |+| r._2)
  }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Monoid` for `NotEmpty` for any type `A`.
  //
  implicit def MonoidNotEmpty[A]: Monoid[NotEmpty[A]] = ???
}

object functor {

  /* Laws:

    Identiry: fmap(identity) === identiry
    Composition: fmap(f.compose(g)) === fmap(f).compose(fmap(g))
  */
  trait Functor_[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def fmap[A, B](f: A => B): F[A] => F[B] = (fa: F[A]) => map(fa)(f)
  }

  // we can only define Functor for Map in this way: fixing one type parameter
  // implicit def FunctorMap[K]: Functor_[Map[K, ?]] = new Functor_[Map[K, ?]] {}

  // Let say we have:
  val g: Int => String = (i: Int) => i.toString()
  val f: String => Int = (s: String) => s.length
  val `f compose g`: Int => Int = f.compose(g)
  val actual = List(1, 19, 200)
  val expect = List(1, 2, 3)

  // Identity law:
  // actual.map(identiry) must be actual
  // fmap(identity)(actual) must be actual

  // Composition law:
  // actual.map(g).map(f) // equivalent for actual.map(f.compose(g))
  // fmap(f)(fmap(g)(actual))

  implicit val FunctorList = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa match {
        case Nil => Nil
        case a :: as => f(a) :: map(as)(f)
      }
  }

  // Let's break some laws:
  // Is there a way to break only identity law?
  // There is no way to break identity law just for identity function.
  // Identity:
  //  - We can return Nil from map
  //  - reverse list in map
  //  - duplicate f(a) :: f(a) :: rest
  // These actions actially breaks all laws. They change structure of returning List[]
  // Any attempt to break identity laws leeds to breaking structure.
  // Lets try to break composition law:
  // - Composition law is broken for set, because set does distinct.
  //

  // refinement types

  // Breaking identity law for Future[_]
  // - You can turn successful future to failed during `map`.

  val OptionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  //
  // EXERCISE 1
  //
  // Define an instance of `Functor` for `BTree`.
  //
  sealed trait BTree[+A]

  case class Leaf[A](a: A) extends BTree[A]

  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]

  implicit val BTreeFunctor: Functor[BTree] =
    new Functor[BTree] {
      def map[A, B](fa: BTree[A])(f: A => B): BTree[B] =
        fa match {
          case Leaf(a) => Leaf(f(a))
          case Fork(l, r) => Fork(map(l)(f), map(r)(f))
        }

    }

  // No sense to have Functor over the map keys. And it break laws.
  def FunctorMap[K]: Functor[Map[K, ?]] = new Functor[Map[K, ?]] {
    // you can't map over keys or over pairs, because it's breaks the laws
    // todo check implementation `mapValues`
    def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = fa.mapValues(f)
  }

  //
  // EXERCISE 2
  //
  // Define an instance of `Functor` for `Nothing`.
  //

  // Nothing is poly-kinded
  // so.. it has any kind you want
  implicit val NothingFunctor: Functor[Nothing] = new Functor[Nothing] {
    def map[A, B](fa: Nothing)(f: A => B): Nothing = fa
  }

  // It may sence to define instances of type classes for some type that contains Nothing as type parameter
  // For example, Either[Nothing, A]
  // You still want to compare it and etc.

  //
  // EXERCISE 3
  //
  // Define an instance of `Functor` for Parser[E, ?].
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])

  implicit def ParserFunctor[E]: Functor[Parser[E, ?]] = new Functor[Parser[E, ?]] {
    def map[A, B](fa: Parser[E, A])(f: A => B): Parser[E, B] = Parser(str =>
      fa.run(str) match {
        case Left(e) => Left(e)
        case Right((st, a)) => Right((st, f(a)))
      }
    )
  }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Functor` for the following data type.
  //
  case class DataType[A](f: A => A)

  implicit val DataTypeFunctor: Functor[DataType] = new Functor[DataType] {
    def map[A, B](fa: DataType[A])(f: A => B): DataType[B] = ???
    // DataType(f.compose(fa.f): A => B) // nothing we can do here,
    // we need function B => B
    // it's called invariant
  }

  // It gives us general rules:
  // Is a TP accurs in contravariant position then this means that you can't define
  // functor for this.

  //
  // EXERCISE 5
  //
  // Define an instance of `Functor` for `FunctorProduct`.
  //
  case class FunctorProduct[F[_], G[_], A](l: F[A], r: F[A])

  implicit def FunctorProductFunctor[F[_] : Functor, G[_] : Functor]:
  Functor[FunctorProduct[F, G, ?]] = ???

  //
  // EXERCISE 6
  //
  // Define an instance of `Functor` for `FunctorSum`.
  //
  case class FunctorSum[F[_], G[_], A](run: Either[F[A], G[A]])

  implicit def FunctorSumFunctor[F[_] : Functor, G[_] : Functor]:
  Functor[FunctorSum[F, G, ?]] = ???

  //
  // EXERCISE 7
  //
  // Define an instance of `Functor` for `FunctorNest`.
  //
  case class FunctorNest[F[_], G[_], A](run: F[G[A]])

  implicit def FunctorNestFunctor[F[_] : Functor, G[_] : Functor]:
  Functor[FunctorNest[F, G, ?]] = ???

  def zipOption[A, B](l: Option[A], r: Option[B]): Option[(A, B)] = (l, r) match {
    case (Some(a), Some(b)) => Some((a, b))
    case _ => None
  }

  def zipWith[A, B, C](l: Option[A], r: Option[B])(f: ((A, B)) => C) =
    zipOption(l, r).map(f)

  def zipList[A, B](l: List[A], r: List[B]): List[(A, B)] = (l, r) match {
    case (a :: as, bs) => zipList(as, bs) ++ bs.map(b => (a, b))
    case (Nil, bs) => Nil
  }

  def zipLis2[A, B](l: List[A], r: List[B]): List[(A, B)] = (l, r) match {
    case (a :: as, b :: bs) => ((a, b)) :: zipLis2(as, bs)
    case _ => Nil
  }

  // zip on Apply take a product and turns it to

  type *[A, B] = (A, B)

  /* todo recap_1 (implement)
  val ApplyOption: Apply[Option] = new Apply[Option] {
    def ap[A, B](fa: => Option[A])(fab: => Option[A => B]): Option[B] = None

    def zip[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = None
  }
  */

  trait Apply_[F[_]] extends Functor[F] {
    def zip[A, B](l: F[A], r: F[B]): F[(A, B)]
  }

  implicit class ApplySyntax[F[_], A](l: F[A]) {
    def *>[B](r: F[B])(implicit F: Apply_[F]): F[B] =
      F.zip(l, r).map(_._2)

    def <*[B](r: F[B])(implicit F: Apply_[F]): F[A] =
      F.zip(l, r).map(_._1)
  }

  trait Applicative_[F[_]] extends Apply_[F] {
    // we have:
    // fa <* point(b) === fa
    // point(b) *> fa === fa

    // we don't have
    // fa <* fb !== fa
    // fa *> fb !== fb
    def point[A](a: => A): F[A] = ???
  }

  // some examples

  val l = List(1, 2, 3)
  val r = List(9, 2)
  val lr1 = List((1, 9), (1, 2), (2, 9), (2, 2), (3, 9), (3, 2))
  val lr2 = List((1, 9), (2, 2))

  val lr1_mapped1 = lr1.map(_._1)

  val lr2_mapped2 = lr1.map(_._2)

  // !In order to define point we have to understand this!
  // (l *> r) !== r
  // (l <* r) !== l

  

  //
  // EXERCISE 8
  //
  // Define `Applicative` for `Option`.
  //
  implicit val OptionApplicative: Applicative[Option] =
  new Applicative[Option] {
    def point[A](a: => A): Option[A] = Some(a)

    def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] =
      (fa, f) match {
        case (Some(a), Some(ff)) => Some(ff(a))
        case _ => None
      }
  }

  //
  // EXERCISE 9
  //
  // Implement `zip` in terms of the applicative composition using `|@|`.
  //
  val example1 = (Option(3) |@| Option(5)) ((_, _))
  val example2 = zip(Option(3), Option("foo")): Option[(Int, String)]

  // todo recap_2
  def zip[F[_] : Applicative, A, B](l: F[A], r: F[B]): F[(A, B)] = (l |@| r) ((_, _))

  // todo recap_3
  def ap2[F[_] : Applicative, A, B](fa: F[A], fab: F[A => B]): F[B] =
    ??? // in terms of zip

  //
  // EXERCISE 10
  //
  // Define an instance of `Applicative` for `Parser[E, ?]`.
  // case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
    new Applicative[Parser[E, ?]] {
      def point[A](a: => A): Parser[E, A] = Parser(input => Right(input, a))

      def ap[A, B](fa: => Parser[E, A])(f: => Parser[E, A => B]): Parser[E, B] = Parser(
        input =>
          f.run(input) match {
            case Left(e) => Left(e)
            case Right((input, f)) => fa.run(input) match {
              case Left(e) => Left(e)
              case Right((input, a)) => Right((input, f(a)))
            }
          }
      )
    }

  // Every Applicative is Apply
  // Every Apply is Functor

  // which means it's possible to have Applicative for function...???
  def pointFunction[A, B](b: => B): A => B = (a: A) => b

  //  trait Monad[F[_]] extends Applicative[F] {
  //    def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  //  }

  implicit val OptionMonad = new Monad[Option] {
    override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa match {
        case Some(a) => f(a)
        case None => None
      }

    override def point[A](a: => A): Option[A] = Some(a)
  }

  implicit val ListMonad = new Monad[List] {
    override def point[A](a: => A): List[A] = List(a)
    override def bind[A, B](fa: List[A])(f: A => List[B]): List[B] = fa match {
      case Nil => Nil
      case a :: xs => f(a) ++ bind(xs)(f)
    }
  }

  //
  // EXERCISE 11
  //
  // Define an instance of `Monad` for `BTree`.
  //
  implicit val MonadBTree: Monad[BTree] = new Monad[BTree] {
    override def point[A](a: => A): BTree[A] = Leaf(a)
    override def bind[A, B](fa: BTree[A])(f: A => BTree[B]): BTree[B] = fa match {
      case Leaf(a) => f(a)
      case Fork(l, r) => Fork(bind(l)(f), bind(r)(f))
    }
  }

  //
  // EXERCISE 12
  //
  // Define an instance of `Monad` for `Parser[E, ?]`.
  //
  implicit def MonadParser[E]: Monad[Parser[E, ?]] = new Monad[Parser[E, ?]] {
    override def point[A](a: => A): Parser[E, A] = Parser(input => Right(input, a))
    override def bind[A, B](fa: Parser[E, A])(f: A => Parser[E, B]): Parser[E, B] = Parser { input =>
      fa.run(input) match {
        case Left(e) => Left(e)
        case Right((inp, a)) => f(a).run(inp)
      }
    }

    // todo parse sequentially
    // zip vs bind
    // in zip we don't depend of on left parser
    def zip[A, B](l: Parser[E, A], r: Parser[E, B]): Parser[E, (A, B)] = ???

  }
}

// Functor - Gives us the power to map values
//           produces by programs without changing their structure
// Apply  -  Gives us the power to combine two
//           programs into one by combining their values
// Applicative = Adds the power to produce a "pure"
//               program that produces a given result
// Monad -   Adds the power to feed the result of one program into a function
//           which can look at runtime value and return a new program, which is used to produce the result of the bind

//def unit: F[Unit]
//def point = unit.map(_ => ...)

// Simigroupal from cats similar to Apply and both of these shouldn't have exist.

object foldable {

  //
  // EXERCISE 1
  //
  // Define an instance of `Foldable` for `BTree`.
  //
  sealed trait BTree[+A]

  case class Leaf[A](a: A) extends BTree[A]

  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]

  val FoldableBTree = new Foldable[BTree] {
    override def foldMap[A, B](fa: BTree[A])(f: A => B)(implicit F: Monoid[B]): B = fa match {
      case Leaf(a) => f(a)
      case Fork(l, r) => foldMap(l)(f) |+| foldMap(r)(f)
    }
    override def foldRight[A, B](fa: BTree[A], z: => B)(f: (A, => B) => B): B = fa match {
      case Leaf(a) => f(a, z)
      case Fork(l, r) => foldRight(l, foldRight(r, z)(f))(f)
    }
  }

  //
  // EXERCISE 2
  //
  // Try to define an instance of `Foldable` for `A => ?`.
  //
  // todo figure out why compilation fails here

//  def FunctionFoldable[A]: Foldable[A => ?] = new Foldable[Function[A, ?]] {
//    override def foldMap[A, B](fa: A => A)(f: A => B)(implicit F: scalaz.Monoid[B]): B = F.zero
//    override def foldRight[A, B](fa: A => A,z: => B)(f: (A, => B) => B): B = z
//  }

  //
  // EXERCISE 3
  //
  // Define an instance of `Traverse` for `BTree`.
  //
  // What is this weird thing?
  // Essence:
  // Sometimes, you are going to have F[G[A]] and you need to get G[F[A]]
  implicit val TraverseBTree: Traverse[BTree] = new Traverse[BTree] {
    override def traverseImpl[G[_], A, B](fa: BTree[A])(f: A => G[B])(
      implicit evidence$1: Applicative[G]): G[BTree[B]] = fa match {
      case Leaf(a) => f(a).map(Leaf(_))
      case Fork(l, r) =>
        val lg: G[BTree[B]] = traverseImpl(l)(f)
        val rg: G[BTree[B]] = traverseImpl(r)(f)

        (lg |@| rg) ((l, r) => Fork(l, r))
    }
  }

  // Traverse if effectfull for loop

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Traverse` for `Parser[E, ?]`.
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])

  implicit def TraverseParser[E]: Traverse[Parser[E, ?]] = ???
}

object optics {

  sealed trait Country {
    val usa: Prism[Country, Unit] = Prism[Country, Unit]({
      case USA => Some(())
      case _ => None
    }, _ => USA)
  }

  case object USA extends Country
  case object UK extends Country

  case class Address(number: Int, street: String, country: Country)
  case class Site(manager: Employee, address: Address, employees: List[Employee])
  object Site {
    def manager: Lens[Site, Employee] = Lens(_.manager, s => _.copy(manager = s))
  }
  case class Employee(name: String, dob: LocalDate, salary: Double, address: Address)
  object Employee {
    def salary: Lens[Employee, Double] = Lens(_.salary, s => _.copy(salary = s))
  }
  case class Org(name: String, address: Address, site: Site)

  object Org {
    val site: Lens[Org, Site] = Lens(_.site, s => _.copy(site = s))
    val salary: Lens[Employee, Double] = Lens(_.salary, s => _.copy(salary = s))
  }

  lazy val orgs: Org = ???
  //  lazy val orgs2 = orgs.copy(
  //    site = orgs.site.copy(manager = )
  //  )

  lazy val orgs2_lens = (Org.site >>> Site.manager >>> Employee.salary).update(_ * 0.95)(orgs)

  // S - Super Structure
  // A - Sub Structure
  // An Optic[S, A] allows you to focus on a sub structure A inside a super structure S,
  // for purposes of
  // accessing or modifying the substructure.
  type Optic[S, A]

  // Lens -
  // Prism - for sum types

  case class Lens[S, A](get: S => A, set: A => (S => S)) {
    self =>
    def >>>[B](that: Lens[A, B]): Lens[S, B] = Lens[S, B](
      get = (s: S) => that.get(self.get(s)),
      set = (b: B) => (s: S) => self.set(that.set(b)(self.get(s)))(s)
    )

    def update(f: A => A): S => S = (s: S) =>
      self.set(f(self.get(s)))(s)

  }

  case class Prism[S, A](
    get: S => Option[A],
    set: A => S) {
    self =>
    def >>>[B](that: Prism[A, B]): Prism[S, B] = Prism[S, B](
      get = (s: S) => self.get(s).flatMap(b => that.get(b)),
      set = self.set.compose(that.set)
    )

    def select() = ???
  }

  def _Left[A, B]: Prism[Either[A, B], A] = Prism[Either[A, B], A](
    {
      case Left(a) => Some(a)
      case _ => None
    }, Left(_))

  def _Right[A, B]: Prism[Either[A, B], B] = ???

  // Optional..
  // todo update elements in list

  // Polymorphic lenses
  // Optic[S, T, A, B]

}
