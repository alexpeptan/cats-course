package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {
  // lists
  val numbersList = List(1,2,3)
  val charsList = List('a', 'b', 'c')
  // TODO 1.1: how would you create all combinations of (number, char)?
  val combinationsList: List[(Int, Char)] =
    numbersList.flatMap(n => charsList.map(c => (n, c)))
  val combinationsListFor: List[(Int, Char)] =
    for {
      n <- numbersList
      c <- charsList
    } yield (n, c)

  // options
  val numberOption = Option(2)
  val charOption = Option('d')
  // TODO 1.2: how do you create the combination of (number, char)?
  val combinationOption = numberOption.flatMap(no => charOption.map(co => (no, co)))
  val combinationOptionFor = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  // above will be the platform on which the futures will be running
  val numberFuture = Future(42) // future that will evaluate to 42 at some point in the future on some thread
  val charFuture = Future('Z') // future that will evaluate to 'z' at some point in the future on some thread
  // TODO 1.3: how do yuu create the combination of (number, char)?
  val combinationFuture = numberFuture.flatMap(nf => charFuture.map(cf => (nf, cf)))
  val combinationFutureFor = for {
    nf <- numberFuture
    cf <- charFuture
  } yield (nf, cf)

  /*
    Pattern:
    - wrapping the value into a monadic value
    - the flatMap mechanism (... transform these M values ... TBC)
      - flatMap is NOT an iteration! (this time). Will INSIST on it throughout the rest of the course!
      - flatMap is a general transformation pattern applicable to many different types
      not just sequential data structures like Lists (Option and Future are not Sequential or serial in any way)
      - BUT flatMap GUARANTEES A SEQUENTIAL ORDER OF EVALUATION

    MONADS - The Cats Type Class that will formalize and embody these 2 capabilities is called a MONAD
  */
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  // Cats Monad
  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  val optionMonad = Monad[Option] // this is the apply method. We have now access to an instance of a Monad typed with Option
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None) // None

  import cats.instances.list._
  val listMonad = Monad[List] // import lit up because the compiler will fetch the implicit TC instance of Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(3, 4)

  // So, the pattern:
  // - Import Cats instances and the particular type for which you want to support Monad
  // - Then you bring it in to scope with Monad[Your Type]
  // - Then you can call the pure and flatMap methods

  // TODO 2: use a Monad[Future]
  import cats.instances.future._
  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(43) // Future(4) // REQUIRE an implicit ExecutionContext -> already existing from above
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x * 2)) // Future that will end up with a Success(86) -> Future(Success(86))

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))
  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))
  // If instead of List we want tu support Options or Futures
  // WE HAVE NO OTHER CHOICE BUT TO DUPLICATE ABOVE CODE!
  // Hence the utility (I deduce) of Monads - an extra level of generalization

  // generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  def main(args: Array[String]): Unit = {
    println(combinationsListFor)
    println(combinationsList)

    println(combinationOption) // I expected the List((2, d)), and instead I got exactly what I wanted...
    println(combinationOptionFor)

    println(combinationFuture)
    println(combinationFutureFor)

    println(anOption)
    println(aTransformedOption)

    println(aTransformedList)

    println(aFuture)
    println(aTransformedFuture)

    println("Using monads: ")
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
//    println(getPairs(numberFuture, charFuture))
    getPairs(numberFuture, charFuture).foreach(println)
  }
}
