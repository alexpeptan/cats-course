package part1intro

object TCVariance {

  import cats.Eq
  import cats.instances.int._ // Eq[Int] TC instance
  import cats.instances.option._ // construct of Eq[Option[Int]] TC instance
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)
//  val anInvalidComparison = Some(2) === None // Eq[Some[Int]] not found although Some is subtype of Option
  // apparently Eq[Some] is not a subtype of Eq[Option] - related to VARIANCE

  // variance
  class Animal
  class Cat extends Animal

  // coveriance type: subtyping propagated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, => Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping propagated BACKWARDS to the generic type
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, => Vet[Animal] <: Vet[Cat]

  // rule of thumb: "HAS a T" = covariant, "ACTS on T", = conrtravariant
  // variance affects how TC instances are being fetched

  // contravariant TC
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("Wow") // implementation not important
  makeSound[Animal] // ok - TC instance defined above
  makeSound[Cat] // ok - TC instance of animal is also applicable for cats
  // rule 1: contravarient TCs can use the superclass instances if nothing is available strictly for that type

  // has implications for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // covariant TC
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }
  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats"
  }
  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show
  // rule 2: covariant TCs will always use the most specific TC instance for that type
  // but may confuse the compiler if the general TC is also present


  // rule 3: you CAN'T have both benefits
  // either 1:
  // - make your general type (TC instance) available for subtypes as well (available for contravariant TCw)
  // or 2:
  // - you have the benefit of picking the most specific TC - which is our case for the coveriant TC

//  Option[2] === Option.empty[Int]

  // Cats uses INVARIANT TCs
  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat]) // ok - container will inject CatsShow as implicit
//    println(organizeShow[Animal]) // will not compile
    /**
     * ambiguous implicit values:
     *  both object GeneralAnimalShow in object TCVariance of type part1intro.TCVariance.GeneralAnimalShow.type
     *  and object CatsShow in object TCVariance of type part1intro.TCVariance.CatsShow.type
     *  match expected type part1intro.TCVariance.AnimalShow[part1intro.TCVariance.Animal]
     *  println(organizeShow[Animal])
     */

    Option[2] === Option.empty[Int]
  }
}
