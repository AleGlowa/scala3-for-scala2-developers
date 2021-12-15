final case class Rational(numerator: BigInt, denominator: BigInt)

extension (r: Rational) {
  private def reduce =
    val divisor = r.numerator gcd r.denominator
    Rational(r.numerator / divisor, r.denominator / divisor)
  def + (otherR: Rational): Rational =
    Rational(
      r.numerator * otherR.denominator + r.denominator * otherR.numerator,
      r.denominator * otherR.denominator
    ).reduce
  def * (otherR: Rational): Rational =
    Rational(
      r.numerator * otherR.numerator,
      r.denominator * otherR.denominator
    ).reduce
  def - (otherR: Rational): Rational =
    Rational(
      r.numerator * otherR.denominator - r.denominator * otherR.numerator,
      r.denominator * otherR.denominator
    ).reduce
}

val r1 = Rational(2, 6)
val r2 = Rational(4, 3)
r1 * r2

extension[A](list: List[A]) def uncons: Option[(A, List[A])] = list.headOption.map((_, list.tail))
val test: Option[(String, List[String])] = List("foo", "bar").uncons

List(1, 2, 3).uncons
Nil.uncons

extension[A](opt: Option[A]) def zip[B](anotherOpt: Option[B]): Option[(A, B)] =
  for a <- opt; b <- anotherOpt yield (a, b)

Some(123).zip(Some("foo"))

extension[A](list: List[Option[A]]) def mapInside[B](f: A => B): List[Option[B]] = list.map(_.map(f))
List(Some("12"), None, Some("321")).mapInside(_.length)
