package part2abstractMath

object Semigroups {

  // Semigroups COMBINE elements of the same type
  // It is a standard of combination between two values
  // We can make use of the presence of the implicit TC Instance of a Semigroup
  // to combine many elements without us needing to specity how to be combined
  import cats.Semigroup
  import cats.instances.int._
  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // addition
  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("I love ", "Cats") // concatenation

  // So this combination function can act as a reducer for Int
  // Specific API
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)


  // general API to reduce anything
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  // TODO 1: support a new type for a semigroup
  case class Expense(id: Long, amount: Double)
  // will create an instance of Semigroup[Expense] to whatever rule of combination I feel is natural
  // hint: used the same pattern I used with Eq - in part1intro - CatsIntro
  // test reduceThings with out list of Expenses


  import cats.instances.double._
  val expenseSemigroupFunction: (Expense, Expense) => Expense =
    (e1, e2) => Expense(Math.max(e1.id, e2.id), Semigroup[Double].combine(e1.amount, e2.amount))
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { expenseSemigroupFunction }

  // extension methods from Semigroup = |+|
  import cats.syntax.semigroup._
  val anIntSum = 2 |+| 3 // requires an implicit Semigroup if Int
  //  val anIntSum = 2 |+| "aString" - does not compile
  val aStringConcat = "Ana" |+| " are mere"
  val aCombinedExpense = Expense(4, 80.99) |+| Expense(2, 23.23)

  // TODO 2: implement reduceThings2 with the |+|
  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T = {
//    list.reduce((a, b) => a |+| b)
    list.reduce(_ |+| _) // same
  }

  def reduceThings3[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    // specific API
    val numbers = (1 to 10).toList
    println(reduceInts(numbers))
    val strings = List("I'm", " starting to like", " semigroups");
    println(reduceStrings(strings))

    // general API
    println(reduceThings(numbers)) // compiler injects the Semigroup[Int]
    println(reduceThings(strings)) // compiler injects the Semigroup[String]
    import cats.instances.option._
    // compiler will produce an implicit Semigroup[Option[Int]] = combine will produce another Option with the summed elements
    // compiler will produce an implicit Semigroup[Option[String]] = combine will produce another Option with the concatenated elements
    // same for any type with an implicit Semigroup
    val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
    println(reduceThings(numberOptions)) // an Option[Int] containing the sum of all the numbers
    val stringOptions: List[Option[String]] = strings.map(s => Option(s))
    println(reduceThings(stringOptions))

    // test ex 1
    val expenseList = List(Expense(12, 3.4), Expense(22, 123.55))
    println(reduceThings(expenseList))
    println(reduceThings2(expenseList))
    println(reduceThings3(expenseList))
  }
}
