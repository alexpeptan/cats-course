package part1recap

// Almost everything in Cats is exposed as a type class
// it's a programming pattern to enhance a type with some form of capability

object TypeClasses {

  case class Person(name: String, age: Int)

  // part 1 - type class definition -> Usually abstract type, trait or abstract class
  trait JSONSerializer[T] {
    def toJSON(value: T): String
  }

  // part 2 - create implicit type class INSTANCES -> or implicit defs, depends on our decision
  // define as singleton because it makes no sense to have more instances of this type
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJSON(value: String): String = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJSON(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJSON(value: Person): String =
      s"""
         |{ "name" : ${value.name}, "age" : ${value.age} }
         |""".stripMargin.trim
  }

  // part 3 - offer some API - given the presence of a serializer for a single value
  def convertListToJSON[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(v => serializer.toJSON(v)).mkString("[", ",", "]")

  // part 4 - extending the existing types via extension methods
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJSON: String = serializer.toJSON(value)
    }
  }

  def main(args: Array[String]): Unit = {
    println(convertListToJSON(List(Person("Alice", 23), Person("Xavier", 45))))
    import JSONSyntax._ // necessary!
    println(Person("Alice", 23).toJSON)
  }
}
