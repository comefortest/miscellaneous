package xyz.comefortest.miscellaneous.example

object SquareExample extends App {

  //a helper for multiplication
  implicit class Multiplication[A](factory: => A) {
    def x(times: Int): List[A] = {
      times match {
        case 1 => List(factory)
        case _ => factory :: x(times - 1)
      }
    }
  }

  //Brick is not a case class, which will do some smart things those I do no want
  class Brick { override def toString = "Brick(" + hashCode + ")" }
  case class Villager(val name: String = "unkown")
  case class P0(val name: String = "unkown") //村长
  case class P1(val name: String = "unkown") //乡长

  case class Square[A](val those: List[A]) {
    def map[B](f: A => B): Square[B] = {
      Square(those.map(f))
    }
    def flatMap[B](f: A => Square[B]) = {
      Square(those.flatMap((x: A) => f(x).those))
    }
    override def toString = "square(" + those.map(_.toString).mkString(", ") + ")"
  }
  object Square {
    def apply[A](those: A*): Square[A] = Square(List(those: _*))
  }

  //广场上站着一个乡长
  val square0 = Square(P1("lazy"))
  println("on the squre0: " + square0)
  //乡长喊来了两个村长, 自己跑了
  val square1 = square0.flatMap((x: P1) => Square(P0("1"), P0("2")))
  println("on the squre1: " + square1)
  //每个村长喊来了两个村民后, 也跑了
  val square2 = square1.flatMap((x: P0) => Square(Villager(x.name + "-1"), Villager(x.name + "-2")))
  println("on the squre2: " + square2)
  //每个村民搬了25000块砖. 累, 回家!
  val square3 = square2.flatMap((x: Villager) => Square(new Brick() x 2 /*25000*/ ))
  println("on the squre3: " + square3)

  //result: 
  //on the squre0: square(P1(lazy))
  //on the squre1: square(P0(1), P0(2))
  //on the squre2: square(Villager(1-1), Villager(1-2), Villager(2-1), Villager(2-2))
  //on the squre3: square(Brick(23141412), Brick(8302170), Brick(15251927), Brick(16842715), Brick(10325673), Brick(72553), Brick(11454213), Brick(9190262))
}