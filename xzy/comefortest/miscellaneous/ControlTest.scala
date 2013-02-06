package xyz.comefortest.miscellaneous.control

object ControlTest extends App {

  import Breakable._
  val outter1 = Breakable[Int]()
  val outter2 = Breakable[Int]()
  val result = outter1 {
    outter2 {
      Some(2)
    }
  }
  for (value <- result) { println("result : " + value) }
}