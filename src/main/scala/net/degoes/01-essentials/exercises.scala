// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.essentials

object types {

  // All the types a  Sets of data.
  // They take up space
  // They have information.

  (i: Int) => i * i
  List((i: Int) => i * i)


  type ??? = Nothing


  // Boolean contains two values True, False

  //
  // EXERCISE 1
  //
  // List all values of the type `Boolean`.
  //
  val BoolValues: List[Boolean] = List(true, false) // it's quite obvious

  // Circle reprsents Set, and dots inside represents elements.

  //
  // EXERCISE 2
  //
  // List all values of the type `Either[Unit, Boolean]`.
  //
  val EitherUnitBoolValues: List[Either[Unit, Boolean]] = List(Right(true), Right(false), Left(()))
  // : operator comes from set theory, that element from the left is from right set

  //
  // EXERCISE 3
  //
  // List all values of the type `(Boolean, Boolean)`.
  //
  val TupleBoolBoolValues: List[(Boolean, Boolean)] = List((true, true), (true, false), (false, false), (false, true))


  // TupleBoolBoolValues isomorphic to EitherEitherUnitUnitUnitValues, they have the same structure

  //
  // EXERCISE 4
  //
  // List all values of the type `Either[Either[Unit, Unit], Unit]`.
  //
  val EitherEitherUnitUnitUnitValues: List[Either[Either[Unit, Unit], Unit]] = List(Right(()), Left(Left(())), Left(Right(())))

  //
  // EXERCISE 5
  //
  // Create a product type of `Int` and `String`, representing the age and
  // name of a person.
  //
  type Person1 = (Int, String)
  case class Person2(age: Int, name: String)


  // Ex 6
  // Prove that `A` * `1` is equivalent to `A`
  // |
  def to1[A](t: (A, Unit)): A = t._1
  def from1[A](a: A): (A, Unit) = (a, Unit) // because Unit caries no information
  // However is you change Unit to Boolean - we are in troule

  //
  // EXERCISE 6
  //
  // Create a sum type of `Int` and `String` representing the identifier of
  // a robot (a number) or a person (a name).
  //

  type Identifier = Either[Int, String] // it's a sum type

  sealed trait Participat
  case class Robot(value: Int) extends Participat
  case class Human(name: String) extends Participat
  val v: Participat = ???
  v match {
    case Robot(v) => ???
    case Human(v) => ???
  }

  // In Scala 3
  /*
  enum Identifier {
    case Robot(value: Int)
    case Human(value: Strng)
  }
  */

  sealed trait ProgrammingLanguage
  case object Scala extends ProgrammingLanguage
  case object Haskell extends ProgrammingLanguage
  case object PureScript extends ProgrammingLanguage
  case object APL extends ProgrammingLanguage

  //
  // Ex 8
  // Prove that `A` + 0 (Nothing) = `A`.
  //
  // We can't produce value of Nothing
  def to2[A](t: Either[A, Nothing]): A = ???
  def from2[A](a: A): Either[A, Nothing] = ???

  // What about Untt?
  def to3[A](t: Either[A, Unit]): A = ???
  def from3[A](a: A): Either[A, Unit] = ???

  def to4[A](t: (A, Unit)): A = ???
  def from4[A](a: A): (A, Unit) = ???

  //
  // EXERCISE 7
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // credit card, which has a number, an expiration date, and a security code.
  //
  case class CreditCard(number: String, cvv: Int)

  //
  // EXERCISE 8
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // payment method, which could be a credit card, bank account, or
  // cryptocurrency.
  //
  sealed trait PaymentMethod
  case object Cash extends PaymentMethod
  case object CreditCard extends PaymentMethod
  case object ApplePay extends PaymentMethod


  //
  // EXERCISE 8
  //
  // Create either a sum type or a product type (as appropriate) to represent an
  // employee at a company, which has a title, salary, name, employment date.
  //
  case class Employee(name: String)

  //
  // EXERCISE 9
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // piece on a chess board, which could be a pawn, rook, bishop, knight,
  // queen, or king.
  //
  sealed trait ChessPiece
  case object Pawd extends ChessPiece
  case object Rook extends ChessPiece
  case object BiShop extends ChessPiece
  case object Knight extends ChessPiece
  case object Queen extends ChessPiece
  case object King extends ChessPiece

  //
  // EXERCISE 10
  //
  // Create an ADT model of a game world, including a map, a player, non-player
  // characters, different classes of items, and character stats.
  //
  case class GameWorld(
    map: GameMap
  )

  class RealmId private (value: Int)
  object RealmId {
    def aplly(id: Int): Option[RealmId] = Some(new RealmId(id))
  }


  // Making

  case class GameMap(realms: List[RealmId], paths: List[(Realm, Realm)])
  case class Realm(items: List[Item])

  sealed trait Item
  case class Sword() extends Item

  /*
  sealed trait GameMap
  case class Dungeon() extends GameMap
  case class Plains() extends GameMap
  */

  // Game map could be also Product type as List[List[Cell]]
  // We actually went too deep desining this game world

}

object functions {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // Convert the following non-function into a function.
  //
  import scala.util.Try
  def parseInt1(s: String): Int = s.toInt
  def parseInt2(s: String): Option[Int] = Try(s.toInt).toOption

  //
  // EXERCISE 2
  //
  // Convert the following non-function into a function.
  //
  def arrayUpdate1[A](arr: Array[A], i: Int, f: A => A): Unit = arr.updated(i, f(arr(i)))

  // Problems
  // This is a partial function
  // that produces side effects
  def arrayUpdate2[A](arr: Array[A], i: Int, f: A => A): Option[Array[A]] =
    if (i >= 0 && i < arr.size) Some(arr) // todo return new arrayUpdate2
    else None

  //
  // EXERCISE 3
  //
  // Convert the following non-function into a function.
  //
  def divide1(a: Int, b: Int): Int = a / b
  def divide2(a: Int, b: Int): Option[Int] = Try(a / b).toOption

  //
  // EXERCISE 4
  //
  // Convert the following non-function into a function.
  //
  var id = 0
  def freshId1(): Int = {
    val newId = id
    id += 1
    newId
  }
  def freshId2(oldId: Int): (Int, Int) = (oldId + 1, oldId)

  //
  // EXERCISE 5
  //
  // Convert the following non-function into a function.
  //
  import java.time.LocalDateTime
  def afterOneHour1: LocalDateTime = LocalDateTime.now.plusHours(1)
  def afterOneHour2(now: LocalDateTime): LocalDateTime = now.plusHours(1)

  //
  // EXERCISE 6
  //
  // Convert the following non-function into function.
  //
  // Probles:
  // - partial
  // - side offect
  def head1[A](as: List[A]): A = {
    if (as.length == 0) println("Oh no, it's impossible!!!")
    as.head
  }

  // Don't write it as
  // if (as.length == 0)
  // because then you have to call as.head which is partial and throw exception
  def head2[A](as: List[A]): Either[String, A] = as match {
    case Nil => Left("Oh no, it's impossible!!!")
    case a :: _ => Right(a)
  }

  //
  // EXERCISE 7
  //
  // Convert the following non-function into a function.
  //
  trait Account
  trait Processor {
    def charge(account: Account, amount: Double): Unit
  }
  case class Coffee() {
    val price = 3.14
  }
  def buyCoffee1(processor: Processor, account: Account): Coffee = {
    val coffee = Coffee()
    processor.charge(account, coffee.price)
    coffee
  }

  // solution
  final case class Charge(acclunt: Account, amount: Double)
  def buyCoffee2(account: Account): (Coffee, Charge) = {
    val coffee = Coffee()
    (coffee, Charge(account, coffee.price))
  }

  //
  // EXERCISE 8
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def printLine(line: String): Unit = ???

  // solution is
  def printLine_(line: String): Unit = ()

  //
  // EXERCISE 9
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def readLine: String = ???
  // any string
  def readLine_(): String = "mikla"

  //
  // EXERCISE 10
  //
  // Implement the following function under the Scalazzi subset of Scala.
  //
  def systemExit(code: Int): Unit = ???
  // well, the same
  def systemExit_(code: Int): Unit = ()

  //
  // EXERCISE 11
  //
  // Rewrite the following non-function `printer1` into a pure function, which
  // could be used by pure or impure code.
  //
  def printer1(): Unit = {
    println("Welcome to the help page!")
    println("To list commands, type `commands`.")
    println("For help on a command, type `help <command>`")
    println("To exit the help page, type `exit`.")
  }

  def printer2[A](println: String => A, combine: (A, A) => A): A = {
    // val first = println("Welcome to the help page!")

    // List("To list commands, type `commands`.", "For help on a command, type `help <command>`","To exit the help page, type `exit`.")
    // .foldLeft(first)(combine)
    ???
  }

  // printer2(println, (_, _) => ())
  // todo try to compile it

  //
  // EXERCISE 12
  //
  // Create a purely-functional drawing library that is equivalent in
  // expressive power to the following procedural library.
  //
  trait Draw {
    def goLeft(): Unit
    def goRight(): Unit
    def goUp(): Unit
    def goDown(): Unit
    def draw(): Unit
    def finish(): List[List[Boolean]]
  }

  def draw1(size: Int): Draw = new Draw {
    val canvas = Array.fill(size, size)(false)
    var x = 0
    var y = 0

    def goLeft(): Unit = x -= 1
    def goRight(): Unit = x += 1
    def goUp(): Unit = y += 1
    def goDown(): Unit = y -= 1
    def draw(): Unit = {
      def wrap(x: Int): Int =
        if (x < 0) (size - 1) + ((x + 1) % size) else x % size

      val x2 = wrap(x)
      val y2 = wrap(y)

      canvas.updated(x2, canvas(x2).updated(y2, true))
    }
    def finish(): List[List[Boolean]] =
      canvas.map(_.toList).toList
  }

  // One way of doing that is to define type
  type Bitmap = List[List[Boolean]]
  type Cursor = (Int, Int)
  type Operation = (Cursor, Bitmap) => (Cursor, Bitmap)

  // then we can define GoLeft, GoRight, that will return (Cursor, Bitmap),
  // it will change cursor, but not bitmap

  val DoRight: Operation = ???
  val DoLeft: Operation = ???
  val DrawOperation: Operation = ???

  // and now we can compose them

  // DoRight.andThen(DoLeft) // todo doesn't compile

  // It's better to define Operation as a sum type
  // sealed trait Operation
  // case object GoLeft extends Operation

  // todo implement this

  // So we switched from function compositions to list of operations.

  def draw2(size: Int, op: List[Operation]): Unit = ???

}

// Higher order functions

object higher_order {
  case class Parser[+E, +A]( // it's nothing more then function
    run: String => Either[E, (String, A)])

  // its can either fail with some error E or it consumes some String and returns A.

  // John wanted to introduce type level development here?

  def fail[E](e: E): Parser[E, Nothing] =
    Parser(_ => Left(e))


  def point[A](a: => A): Parser[Nothing, A] =
    Parser(in => Right((in, a)))


  def char[E](e: E): Parser[E, Char] =
    Parser(input => input.toList match {
      case Nil => Left(e)
      case char :: chars => ??? // what to return here?
                                // type doesn't say me enoght
    })

  //
  // EXERCISE 1
  //
  // Implement the following higher-order function.
  //

  // Just play functional programming game
  def alt[E1, E2, A, B](l: Parser[E1, A], r: Parser[E2, B]):
    Parser[E2, Either[A, B]] = Parser(input =>
      l.run(input) match {
        case Left(e) => r.run(input) match {
          case Left(e2) => Left(e2)
          case Right((in, b)) => Right((in, Right(b)))
        }
        case Right((str, a)) => Right((str, Left(a)))
      }
    )

  //
  // EXERCISE 2
  //
  // Implement the following higher-order function.
  //
  def fanout[A, B, C](f: A => B, g: A => C): A => (B, C) = a => (f(a), g(a))

  //
  // EXERCISE 3
  //
  // Implement the following higher-order function.
  //
  def parallel[A, B, C, D](f: A => B, g: C => D): (A, C) => (B, D) = (a, c) => (f(a), g(c))

  //
  // EXERCISE 4
  //
  // Implement the following higher-order function.
  //
  def leftChoice[A, B, C](f: A => B, g: C => B): Either[A, C] => B = c => c match {
    case Left(a) => f(a)
    case Right(c) => g(c)
  }

  //
  // EXERCISE 5
  //
  // Implement the following higher-order function.
  //
  def choice[A, B, C, D](f: A => B, g: C => D): Either[A, C] => Either[B, D] = aorc =>
    aorc match {
      case Left(a) => Left(f(a))
      case Right(c) => Right(g(c))
    }

  //
  // EXERCISE 6
  //
  // Implement the following higer-order function.
  //
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))


  // Types are not values,
  // we can't pass them around functions as values
  // takeType(Boolean) not gonna work
  //     | type param slot |
  def alt1[E1, E2, A, B](l: Parser[E1, A], r: Parser[E2, B]): Parser[E2, Either[A, B]] = ???
}


// The more polymorphic function we have...

object poly_functions {
  //
  // EXERCISE 1
  //
  // Create a polymorphic function called `snd` that returns the second
  // element out of any `(A, B)`.
  //
  object snd {
    def apply[A, B](t: A, b: B) = b
  }
  snd(1, "foo") // "foo"

  //
  // EXERCISE 2
  //
  // Create a polymorphic function called `repeat` that can take any
  // function `A => A`, and apply it repeatedly to a starting value
  // `A` the specified number of times.
  //
  object repeat {
    // it's polymorphic and higher-order function
    def apply[A](n: Int)(init: A, f: A => A): A =
      if (n <= 0) init
      else repeat(n - 1)(f(init), f)
  }
  repeat[Int](100)(0, _ + 1) // 100
  repeat[String](10)("", _ + "*") // "**********"

  //
  // EXERCISE 3
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample1[A, B](a: A, b: B): Either[A, B] = ???
  val countExample1Answer = ???

  //
  // EXERCISE 4
  //
  // Count the number of unique implementations of the following method.
  //
  def countExample2[A, B](f: A => B, g: A => B, a: A): B = ???
  val countExample2Answer = ???

  //
  // EXERCISE 5
  //
  // Implement the function `groupBy`.
  //
  val Data =
    "poweroutage;2018-09-20;level=20" :: Nil

  val By: String => String =
    (data: String) => data.split(";")(1)

  val Reducer: (String, List[String]) => String =
    (date, events) =>
      "On date " +
        date + ", there were " +
        events.length + " power outages"

  val Expected =
    Map("2018-09-20" ->
      "On date 2018-09-20, there were 1 power outages")

  // todo implementat

  // Java programmer could say "omg, it's beautiul", it's so concreate
  def groupBy1(
    l: List[String],
    by: String => String)(
      reducer: (String, List[String]) => String):
      Map[String, String] = ???
        // l.map(value => (value, by(value)))
        // .map {
          // case (initialData, date) => reducer(date)
        // }


  // LEt's consiter all ways that groupBy1 can go wrong
  // - I screw data easily here


  groupBy1(Data, By)(Reducer) == Expected

  //
  // EXERCISE 6
  //
  // Make the function `groupBy1` as polymorphic as possible and implement
  // the polymorphic function. Compare to the original.
  //
  object groupBy2 {
    // here we are the sitiation when it's harder to screw thing up.
    // why so complex you may ask.
    // but it's not
    // here you just "following the types"
    def apply[A, B, C](
      l: List[A], // we can go deeper List[] can be F[]
      by: A => B)(reducer: (B, List[A]) => C): Map[B, C] =
        l.groupBy(by).map {
          case (a, b) => a -> reducer(a, b)
        }
  }
}

object higher_kinded {
  type ?? = Nothing
  type ???[A] = Nothing
  type ????[A, B] = Nothing
  type ?????[F[_]] = Nothing

  trait `* => *`[F[_]]
  trait `[*, *] => *`[F[_, _]]
  trait `(* => *) => *`[T[_[_]]]

  // List type c

  //
  // EXERCISE 1
  //
  // Identify a type constructor that takes one type parameter (i.e. has kind
  // `* => *`), and place your answer inside the square brackets.
  //
  type Answer1 = `* => *`[???] // List

  //
  // EXERCISE 2
  //
  // Identify a type constructor that takes two type parameters (i.e. has kind
  // `[*, *] => *`), and place your answer inside the square brackets.
  //
  type Answer2 = `[*, *] => *`[????] // Either


  //
  // EXERCISE 3
  //
  // Create a new type that has kind `(* -> *) -> *`.
  //
  type NewType1 /* ??? */
  type Answer3 = `(* => *) => *`[?????]

  //
  // EXERCISE 4
  //
  // Create a trait with kind `*`.
  //
  trait Answer4 /*[]*/

  //
  // EXERCISE 5
  //
  // Create a trait with kind `[*, *, *] => *`.
  //
  trait Answer5 /*[]*/ // Tuple3

  //
  // EXERCISE 6
  //
  // Create a trait with kind `[* => *, (* => *) => *] => *`.
  //
  trait Answer6 /*[]*/ // Answer6[A[_], B[_[_]]]

  //
  // EXERCISE 7
  //
  // Create an implementation of the trait `CollectionLike` for `List`.
  //

  // todo implement
  trait CollectionLike[F[_]] {
    def empty[A]: F[A]

    def cons[A](a: A, as: F[A]): F[A]

    def uncons[A](as: F[A]): Option[(A, F[A])]

    final def singleton[A](a: A): F[A] =
      cons(a, empty[A])

    final def append[A](l: F[A], r: F[A]): F[A] =
      uncons(l) match {
        case Some((l, ls)) => append(ls, cons(l, r))
        case None => r
      }

    final def filter[A](fa: F[A])(f: A => Boolean): F[A] =
      bind(fa)(a => if (f(a)) singleton(a) else empty[A])

    final def bind[A, B](fa: F[A])(f: A => F[B]): F[B] =
      uncons(fa) match {
        case Some((a, as)) => append(f(a), bind(as)(f))
        case None => empty[B]
      }

    final def fmap[A, B](fa: F[A])(f: A => B): F[B] = {
      val single: B => F[B] = singleton[B](_)

      bind(fa)(f andThen single)
    }
  }
  val ListCollectionLike: CollectionLike[List] = ???

  //
  // EXERCISE 8
  //
  // Implement `Sized` for `List`.
  //
  trait Sized[F[_]] {
    // This method will return the number of `A`s inside `fa`.
    def size[A](fa: F[A]): Int
    // def size(fa: F[_]): Int (existential types that are going away in Scala 3)
    // todo read about it
  }
  val ListSized: Sized[List] = new Sized[List] {
    def size[A](fa: List[A]): Int = fa.size
  }

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to `String`.
  //
  val MapSized1: Sized[Map[String, ?]] = new Sized[Map[String, ?]] {
    def size[A](fa: Map[String, A]) = fa.size
  }

  // In scala 3 you will use Map[String, _]

  //
  // EXERCISE 9
  //
  // Implement `Sized` for `Map`, partially applied with its first type
  // parameter to a user-defined type parameter.
  //
  def MapSized2[K]: Sized[Map[K, ?]] = new Sized[Map[K, ?]] {
    def size[A](fa: Map[K, A]): Int = fa.size
  }
  MapSized2[String].size[Int](Map("foot" -> 1))

  //
  // EXERCISE 10
  //
  // Implement `Sized` for `Tuple3`.
  //
  def Tuple3Sized[A, B]: Sized[Tuple3[A, B, ?]] = new Sized[Tuple3[A, B, ?]] {
    def size[C](t: Tuple3[A, B, C]) = 1 // number of C-s in F[]
  }

}

object typeclasses {

  // write a sort for List[Int]
  // remote Int, this means we promise to sort List of any A
  // which hard, we made promise that we can sort List of functions, Spark RDD or Java Factory Beans
  // now we have to have aditiona function lt: (A, A) => Boolean
  // we can use it in partition function
  // xs.partition(lt(_, x))

  // If we have a1: A, a2: A, a3: A
  // Then:
  // lt(a1, a2) && lt(a2, a3) == lt(a1, a3) (transitivity)
  // lt(a1, a1) = false
  // So, the function lt should imply some rules (laws)

  // It's becoming incovenient to say about these laws every time.
  // Also, carring this functions also quite not handy

  // type class is:
  // A type class is a set if three things
  // 1. Types
  // 2. Operations on values of those types
  // 3. Laws govering the operations

  /*
  Type classes are encoded with Traits in Scala
  1. The types are the type parameters of the trait.
  2. The oprations are the method of the trait
  3. Operations laws, if you have no laws - it's just obfuscation.

  */


  /**
   * {{
   * Reflexivity:   a ==> equals(a, a)
   *
   * Transitivity:  equals(a, b) && equals(b, c) ==>
   *                equals(a, c)
   *
   * Symmetry:      equals(a, b) ==> equals(b, a)
   * }}
   */
  trait Eq[A] {
    def equals(l: A, r: A): Boolean
  }

  def num[A](list: List[A])(eq: Eq[A]): List[A] = {
    def contains(a1: A, l: List[A]): Boolean =
      l.foldLeft(false)((b, a2) => b || eq.equals(a1, a2))

    // or foldLeft and then reverse
    list.foldRight[List[A]](Nil) {
      case (a, acc) =>
        if (contains(a, acc)) acc
        else a :: acc
    }
  }

  // We can now define Eq[List[A]]
  // todo implement
  // - use implicits instead of passing Eq[]s manually
  // - then use context bound
  // - it will require implicitly[]
  // Use implicit class to eliminate this implicitly[]
  // - implement === in EqualsSyntax


  object Eq {
    def apply[A](implicit eq: Eq[A]): Eq[A] = eq

    implicit val EqInt: Eq[Int] = new Eq[Int] {
      def equals(l: Int, r: Int): Boolean = l == r
    }
    implicit def EqList[A: Eq]: Eq[List[A]] =
      new Eq[List[A]] {
        def equals(l: List[A], r: List[A]): Boolean =
          (l, r) match {
            case (Nil, Nil) => true
            case (Nil, _) => false
            case (_, Nil) => false
            case (l :: ls, r :: rs) =>
              Eq[A].equals(l, r) && equals(ls, rs)
          }
      }
  }
  implicit class EqSyntax[A](val l: A) extends AnyVal {
    def === (r: A)(implicit eq: Eq[A]): Boolean = // <- why we didn't add implicit to A? to consturctor
      eq.equals(l, r)                             // because of error message
                                                  // todo check it at home
  }

  // And then we are writing sort with Ord type class
  // todo implement it
  // use partition(a => a =?= b === LT)
  // Also, define Eq[Ordering]
  // Also, we can extend our syntax to use <, >, <=, >=
  def sort2_[A: Ord](l: List[A]): List[A] = ???


  //
  // Scalaz 7 Encoding
  //
  sealed trait Ordering
  case object EQUAL extends Ordering
  case object LT extends Ordering
  case object GT extends Ordering
  object Ordering {
    implicit val OrderingEq: Eq[Ordering] = new Eq[Ordering] {
      def equals(l: Ordering, r: Ordering): Boolean =
        (l, r) match {
          case (EQUAL, EQUAL) => true
          case (LT, LT) => true
          case (GT, GT) => true
          case _ => false
        }
    }
  }

  trait Ord[A] {
    def compare(l: A, r: A): Ordering
  }
  object Ord {
    def apply[A](implicit A: Ord[A]): Ord[A] = A

    implicit val OrdInt: Ord[Int] = new Ord[Int] {
      def compare(l: Int, r: Int): Ordering =
        if (l < r) LT else if (l > r) GT else EQUAL
    }
  }
  implicit class OrdSyntax[A](val l: A) extends AnyVal {
    def =?= (r: A)(implicit A: Ord[A]): Ordering =
      A.compare(l, r)

    def < (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), LT)

    def <= (r: A)(implicit A: Ord[A]): Boolean =
      (l < r) || (this === r)

    def > (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), GT)

    def >= (r: A)(implicit A: Ord[A]): Boolean =
      (l > r) || (this === r)

    def === (r: A)(implicit A: Ord[A]): Boolean =
      Eq[Ordering].equals(A.compare(l, r), EQUAL)

    def !== (r: A)(implicit A: Ord[A]): Boolean =
      !Eq[Ordering].equals(A.compare(l, r), EQUAL)
  }
  case class Person(age: Int, name: String)
  object Person {
    implicit val OrdPerson: Ord[Person] = new Ord[Person] {
      def compare(l: Person, r: Person): Ordering =
        if (l.age < r.age) LT else if (l.age > r.age) GT
        else if (l.name < r.name) LT else if (l.name > r.name) GT
        else EQUAL
    }
    implicit val EqPerson: Eq[Person] = new Eq[Person] {
      def equals(l: Person, r: Person): Boolean =
        l == r
    }
  }

  //
  // EXERCISE 1
  //
  // Write a version of `sort1` called `sort2` that uses the polymorphic `List`
  // type constructor, and which uses the `Ord` type class, including the
  // compare syntax operator `=?=` to compare elements.
  //
  def sort1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case x :: xs =>
      val (lessThan, notLessThan) = xs.partition(_ < x)

      sort1(lessThan) ++ List(x) ++ sort1(notLessThan)
  }
  def sort2[A: Ord](l: List[A]): List[A] = ???

  //
  // Scalaz 8 Encoding
  //
  sealed abstract class InstanceOfModule {
    type InstanceOf[T] <: T
    def instanceOf[T](t: T): InstanceOf[T]
  }

  object InstanceOfModule {
    val impl: InstanceOfModule = new InstanceOfModule {
      override type InstanceOf[T] = T
      override def instanceOf[T](t: T) = t
    }
  }
  import InstanceOfModule.impl._

  type ???[A] = Nothing

  /**
   * {{
   * // Associativity:
   * (a <> b) <> c === a <> (b <> c)
   * }}
   */
  trait SemigroupClass[A] {
    def append(l: => A, r: => A): A
  }
  type Semigroup[A] = InstanceOf[SemigroupClass[A]]
  object SemigroupClass {
    def apply[A](implicit A: Semigroup[A]): Semigroup[A] = A

    implicit val SemigroupString: Semigroup[String] =
      instanceOf(
        new SemigroupClass[String] {
          def append(l: => String, r: => String): String = l + r
        })
    implicit def SemigroupList[A]: Semigroup[List[A]] =
      instanceOf(
        new SemigroupClass[List[A]] {
          def append(l: => List[A], r: => List[A]): List[A] = l ++ r
        })
  }
  implicit def AnyToSemigroupSyntax[A](a: => A): SemigroupSyntax[A] =
    new SemigroupSyntax(() => a)
  class SemigroupSyntax[A](l: () => A) {
    def <> (r: => A)(implicit A: Semigroup[A]): A = A.append(l(), r)
  }
  //
  // EXERCISE 2
  //
  // Create an instance of the `Semigroup` type class for `java.time.Instant`.
  //
  implicit val SemigroupInstant: Semigroup[java.time.Instant] = ???

  //
  // EXERCISE 3
  //
  // Create an instance of the `Semigroup` type class for `Int`.
  //
  implicit val SemigroupInt: Semigroup[Int] = ???

  //
  // EXERCISE 4
  //
  // Create an instance of the `Semigroup` type class for `Set[A]`.
  //
  implicit def SemigroupSet[A]: Semigroup[Set[A]] = ???

  //
  // EXERCISE 5
  //
  // Create an instance of the `Semigroup` type class for `Map[K, ?]`. Hint:
  // you will need some constraint applied to the values.
  //
  implicit def SemigroupMap[K, V: ???]: Semigroup[Map[K, V]] =
    ???

  //
  // EXERCISE 6
  //
  // Create a type class `Monoid[A]` that implies `Semigroup[A]` (that is, every
  // `Monoid[A]` must be a `Semigroup[A]`), which adds a single operation called
  // `zero`, which satisfies additional laws.
  //
  /**
   * {{
   * append(zero, a) === a
   * append(a, zero) === a
   * }}
   */
  trait MonoidClass[A] extends SemigroupClass[A] {
    /* ??? */
  }
  object MonoidClass {
    def apply[A](implicit A: Monoid[A]): Monoid[A] = ???
  }
  type Monoid[A] = InstanceOf[MonoidClass[A]]
  implicit def MonoidSemigroup[A](implicit M: Monoid[A]): Semigroup[A] =
    instanceOf(M)
  def empty[A: Monoid]: A = ???

  //
  // EXERCISE 7
  //
  // Create an instance of the `Monoid` type class for `java.time.Instant`.
  //
  implicit val MonoidInstant: Monoid[java.time.Instant] = ???

  //
  // EXERCISE 8
  //
  // Create an instance of the `Monoid` type class for `String`.
  //
  implicit val MonoidString: Monoid[String] = ???

  //
  // EXERCISE 9
  //
  // Create an instance of the `Monoid` type class for `List[A]`.
  //
  implicit def MonoidList[A]: Monoid[List[A]] = ???

  //
  // EXERCISE 10
  //
  // Create an instance of the `Monoid` type class for `Int`.
  //
  implicit val MonoidInt: Monoid[Int] = ???

  //
  // EXERCISE 11
  //
  // Using a newtype, create an instance of the `Monoid` type class for `Int`
  // representing the additive monoid, with addition as `append`, and 0 as
  // `zero`.
  //
  final case class Sum(run: Int)
  implicit val MonoidSum: Monoid[Sum] = ???

  //
  // EXERCISE 12
  //
  // Using a newtype, create an instance of the `Monoid` type class for `Int`
  // representing the multiplicative monoid, with multiplication as `append`,
  // and 1 as `zero`.
  //
  final case class Product(run: Int)
  implicit val MonoidProduct: Monoid[Product] = ???

  //
  // EXERCISE 13
  //
  // Create an instance of the `Collection` type class for `List`.
  //
  trait CollectionClass[F[_]] {
    def empty[A]: F[A]
    def cons[A](a: A, as: F[A]): F[A]
    def uncons[A](fa: F[A]): Option[(A, F[A])]
  }
  object CollectionClass {
    def apply[F[_]](implicit F: Collection[F]): Collection[F] = F
  }
  type Collection[F[_]] = InstanceOf[CollectionClass[F]]
  implicit val ListCollection: Collection[List] = ???
}
