package part1intro

object Implicits {

  // implicit classes
  case class Person(name:String) {
    def greet: String = s"Hi, My name is $name"
  }

  // implicit classes take a single arg all the time!
  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

//  val impersonableString = new ImpersonableString("Peter")
//  impersonableString.greet

  // or:
  val greeting = "Peter".greet // works because the above class is implicit!
  // new ImpersonableString("Peter").greet
  // this pattern is called an extension method because the method does
  // not belong to the original class, but through an implicit method and
  // an implicit class it does.

  // importing implicit conversions in scope
  import scala.concurrent.duration._
  val oneSec = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount = 10
  val incremented2 = increment(2)//(10) // implicit argument 10 is passed by the compiler

  def multiply(x: Int)(implicit times: Int) = x * times
  val times2 = multiply(2)

  // more complex example
  trait JSONSerializer[T] {
    def toJson(value:T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person): String =
      s"""
         |{"name" : "${person.name}"}
         |""".stripMargin
  }
  val personsJson = listToJson(List(Person("Alice"), Person("Bob")))//(personSerializer)
  // implicit argument is used to PROVE THE EXISTENCE of a type

  // implicit methods
  // we'll need to define serializers (like personSerializer)
  // for any type that we might need to use
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |{ "${value.productElementName(0)}" : "${value.productElement(0)}" }
         |""".stripMargin.trim
  }

  // Cat, being a case class will extend Product!!!
  // Compiler automatically creates a JSONSerializer[Cat] for me!!
  //
  case class Cat(catName: String)

  // So, this oneArgCaseClassSerializer is applicable both for Cat and Person
  // and will be applicable to any case class with one argument

  // My question: what happens if I try to use this serializer to a case class with 2 args?
  // I'd assume that the second one gets ignored, no biggie
  case class PersonDetails(name: String, age: Int)

  val catsToJson = listToJson(List(Cat("Ana"), Cat("Diana")))
  // in the background: val catsToJson = listToJson(List(Cat("Ana"), Cat("Diana")))(oneArgCaseClassSerializer[Cat])
  // implicit methods are used to PROVE THE EXISTENCE of a type

  // can be used as a conversion(DISCOURAGED)
  // RATHER WE SHOULD USE AN IMPLICIT CLASS TO GRANT AN EXISTING TYPE WITH
  // ADDITIONAL METHODS, like greet method for Strings on line 11

  def main(args: Array[String]): Unit = {
    println(oneArgCaseClassSerializer[Cat])
    println(oneArgCaseClassSerializer[Cat].toJson(Cat("Garfield")))
    println(oneArgCaseClassSerializer[Person].toJson(Person("David")))
    println(oneArgCaseClassSerializer[PersonDetails].toJson(PersonDetails("David the young", 22)))
    println(catsToJson)
  }
}
