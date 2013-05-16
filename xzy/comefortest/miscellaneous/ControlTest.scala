package xyz.comefortest.miscellaneous.control

object ControlTest extends App {
  import xyz.comefortest.miscellaneous.control._
  import Breakable._

  println("test0 ...")
  val r0 = breakable {
    val now = System.currentTimeMillis()
    now % 5 match {
      case 0 => Some(0)
      case 1 => break(Option("abc"))
      case 2 => break
      case _ => Some(now)
    }
  }
  println("r0: " + r0)

  println("test1 ...")
  val outter_x = Breakable[String]()
  val outter_y = Breakable[Long]()
  val r1 = outter_x {
    outter_y {
      val now = System.currentTimeMillis()
      now % 5 match {
        case 0 => Some(0l)
        case 1 => outter_x.break(Option("abc"))
        case 2 => break //WRONG
        case 3 => outter_y.break(Option(999))
        case _ => Some(now)
      }
    }.map(_.+(1).toString)
  }
  println("r1: " + r1)
}