package part2abstractMath

import cats.Semigroup
import part2abstractMath.Functors.treeFunctor
import part2abstractMath.Semigroups.Expense

import scala.util.Try

object Functors {

  val aModifiedList = List(1,2,3).map(_ + 1) // List(2,3,4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry = Try(42).map(_ + 1) // Success(43)

  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._ // includes Functor[List]
  val listFunctor = Functor[List]
  val  incrementedNumbers = listFunctor.map(List(1,2,3))(_ + 1) // List(2,3,4)

  import cats.instances.option._ // includes Functor[Option]
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(43))(_ + 1) // Some(3)

  import cats.instances.try_._
  val anIncrementedTry = Functor[Try].map(Try(42))(_ + 1) // Success(43)

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  // generalize
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  // TODO 1: define your own Functor for a binary tree - and test the do10x API on your own binary tree
  // hint: define an object which extends Functor[Tree] -> map method from above

  trait Tree[+T]
  // or build some smart constructors
  object Tree {
    // "smart" constructors - they return the general Type Tree instead of the one from subclasses
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  // will define implicit for functor[Tree]
  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      tree match {
        case Leaf(v) => Leaf(f(v))
        case Branch(v, left, right) => Branch(f(v), map(left)(f), map(right)(f))
      }
  }
  val treeFunctor = Functor[Tree]

  // extension method - map
  import cats.syntax.functor._ // allows me to call map on my data structure AS LONG AS I HAVE A FUNCTOR OF THAT TYPE IN SCOPE
  val tree: Tree[Int] = Tree.branch(40, Tree.branch(4, Tree.leaf(10), Tree.leaf(2)), Tree.leaf(44))
  val incrementedTree = tree.map(_ + 1)

  // TODO 2: Create a shorter version of do10x - do10xshorter using extension methods
  def do3xShorter[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 3)
  // F[_] : Functor -> the context bound restriction -> there must be an implicit Functor in scope
  def main(args: Array[String]): Unit = {
    println(do10x(List(1,2,3)))
    println(do10x(Option(1)))
    println(do10x(Try(12)))

    val tree: Tree[Int] = Branch(12, Leaf(2), Leaf(33))
    val tree2: Tree[Int] = Branch(12, Branch(12, Branch(12, Leaf(2), Leaf(33)), Leaf(33)), Branch(12, Leaf(2), Branch(12, Leaf(2), Leaf(33))))
    val tree3: Tree[Int] = Leaf(2)

    println(do10x(tree))
    println(do10x(tree2))
    println(do10x(tree3))
    // Or just ise do10x[Tree](...)
    println(do10x[Tree](Leaf(77)))
    // Or just use the freshly defined smart constructors:
    println(do10x(Tree.branch(12, Tree.leaf(3), Tree.leaf(54))))

    println(incrementedTree)
    print(do3xShorter(incrementedTree))
  }
}
