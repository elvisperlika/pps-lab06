object Test {

  enum List[A]:
    case ::(h: A, t: List[A])
    case Nil()
    def ::(h: A): List[A] = List.::(h, this)
    def isEmpty: Boolean = this == Nil()

  object List:
    def head(l: List[Int]): Option[Int] = l match {
      case ::(h, t) => Some(h)
      case _ => None
    }
}

@main
def main(): Unit = {
  import Test.*
  import List.*

  val list: List[Int] = ::(10, Nil())
  var list2: List[Int] = 10 :: 20 :: 30 :: Nil()
  val list3: List[String] = Nil()
  println(list3.isEmpty)

}