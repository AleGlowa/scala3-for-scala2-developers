def getT[T: scala.reflect.Typeable](list: List[Any]): Option[T] =
  list match
    case (head: T) :: _ => Some(head)
    case _ => None

val l = 2.1 :: "s" :: null :: None :: Int.MaxValue :: Nil
getT[Int](l)

val h :: t :: s = ::("bar", ::("foo", Nil))
s

for
  (l, r) <- Some((19, 42))
  case x :: xs <- Some(List(1, 2, 3))
  case Right(v) <- Some(Either.cond(4 > 3, "it works", "what?"))
yield
  s"$v ${l + r + x + xs.sum}"
