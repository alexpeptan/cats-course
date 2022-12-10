package part2abstractMath

import part2abstractMath.Monoids.ShoppingCart
import part2abstractMath.Semigroups.Expense

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // import the "combine" |+| extension method

  val numbers = (1 to 1000).toList
  // |+| is associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
//  def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
//    list.foldLeft(/* WHAT? */)(_ |+| _)

  // I think we'll need a neutral element. One that applied to a value via |+| either on the left or right side leaves the element unchanged -> We'll see

  // MONOIDS
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999)
  val zero = intMonoid.empty // 0

  import cats.instances.string._ // bring the implicit Monoid[String]
  val emptyString = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("I understand", " monoids")

  import cats.instances.option._ // constructs an implicit Monoid[Option[Int]]
  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOptions = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)
  val combineOptions2 = Monoid[Option[Int]].combine(Option(3), Option(6)) // Some(9)

  // extension method for monoids - |+|
//  import cats.syntax.monoid._ // or cats.syntax.semigroup._
  val combinedOptionFancy = Option(3) |+| Option(7) // Some(10)

  // TODO 1: implement a reduceByFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  // TODO 2: combine a list of Phonebooks Maps[String, Int]
  // combine via combineFold and an implicit Monoid[Map[String, Int]]
  // hint: no need to construct the monoid, will import
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 432
    ),
    Map(
      "Geo" -> 43,
      "Xavier" -> 544
    ),
    Map(
      "Alex" -> 77
    )
  )

  import cats.instances.map._ // Imports the monoid of a map - empty element empty Map & combine of maps is a map combining the associations of the maps

  // TODO 3: shopping cart and online stores with monoid
  case class ShoppingCart(items: List[String], total: Double)

  // hint 1: so I'll have to implement a monoid of ShoppingCarts - Monoid.instance
  // hint 2: use combineByFold
  val shoppingCartMonoidFunction: (ShoppingCart, ShoppingCart) => ShoppingCart =
    (c1, c2) => ShoppingCart(c1.items ++ c2.items, c1.total + c2.total)
  private val shoppingCartNeutralElement: ShoppingCart = ShoppingCart(List.empty, 0)
  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](shoppingCartNeutralElement, shoppingCartMonoidFunction)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight) // should be identical because of the associativity property of |+|

    val first10 = (1 to 10).toList
    println(combineFold(first10))
    println(combineFold(List("I ", " like", " monoids")))
    val massivePhonebook = combineFold(phonebooks)
    println(massivePhonebook)

    val cart1: ShoppingCart = ShoppingCart(List("Ace", "brice", "carice"), 99)
    val cart2: ShoppingCart = ShoppingCart(List("mere", "micsunele"), 12.3)
    val allCarts: List[ShoppingCart] = List(cart1, cart2)

    val cartsCombined = combineFold(allCarts)
    println(cartsCombined)
  }
}
