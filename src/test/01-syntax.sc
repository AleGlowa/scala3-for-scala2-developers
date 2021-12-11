def joke(v: String) =
  v match
    case "knock, knock" => println("Who's there?")
    case _ => println("Unknown input!")

def whatIsYourName =
  println("What is your name?")
  val name = scala.io.StdIn.readLine()
  println(s"Hello, ${name}!")

def tryItAndCatchIt =
  try
    throw new IllegalStateException("Wyoming")
  catch
    case _ : IllegalStateException => println("That state is illegal!")

def forComprehension =
  val numbers = List(1, 2, 9, 3, -1, 6, 5, 2)

  for
    number1 <- numbers
    number2 <- numbers
    if ((number1 - number2).abs == 2)
  yield (number1, number2)

def whileLoop(n: Int) =
  var i = 0

  while (i < n)
    println("All work and no play makes Jack a dull boy")
    i = i + 1

enum PaymentMethod:
  case Amex
  case Visa
  case MasterCard

def isItAmex(pm: PaymentMethod): String =
  if pm == PaymentMethod.Amex then "It's an amex!" else "It's something else!"

final case class Person(name: String, friends: List[Person])

Person("Sherlock Holmes", Nil) match
  case Person(name, friends*) => println(friends)

def listFriends(ps: Person*): Unit =
  ps.foreach {
    case Person("name1", _) => println("name1")
    case Person(_, fs*) => println(fs.mkString(", "))
  }

val firstPerson = Person("name1", Nil)
val secondPerson = Person("Arnold", firstPerson :: Nil)
val thirdPerson = Person("Sherlock Holmes", firstPerson :: secondPerson :: Nil)

listFriends(firstPerson, secondPerson, thirdPerson)
