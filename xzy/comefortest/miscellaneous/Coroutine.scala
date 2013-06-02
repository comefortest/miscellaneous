package xyz.comefortest.miscellaneous.control

import scala.util.continuations._

final class Coroutine(allWork: (Unit => Unit)) {
  private var work: (Unit => Unit) = if (null == allWork) (z: Unit) => {} else allWork
}

object Coroutine {
  def apply(x: => Unit @scala.util.continuations.cpsParam[Unit, Unit]) = {
    new Coroutine((z: Unit) => reset { x })
  }
  private final case class RException(r: Coroutine, k: Unit => Unit) extends Exception
  //执行Coroutine
  def execute(r: Coroutine) = {
    var now: Coroutine = r
    while (null != now) {
      try {
        now.work()
        now = null
      } catch {
        case RException(r, k) => {
          now.work = k //最新剩余的工作
          now = r
        }
      }
    }
  }
  //将控制权交给另一个Coroutine
  def switchTo(r: Coroutine) = shift { k: (Unit => Unit) =>
    {
      throw RException(r, k)
    }
  }
}

object CoroutineTest extends App {

  //测试代码
  var g1: Coroutine = null
  var g2: Coroutine = null
  var g3: Coroutine = null

  import Coroutine._

  g1 = Coroutine {
    println(1)
    switchTo(g2)
    println(2)
    switchTo(g2)
  }
  g2 = Coroutine {
    println(3)
    switchTo(g3)
    println(4)
    switchTo(g3)
  }
  g3 = Coroutine {
    println(5)
    switchTo(g1)
    println(6)
    switchTo(g1)
  }
  execute(g1)
  g1 = Coroutine {
    println(98)
    switchTo(g1)
    println(99)
    switchTo(g1)
    println(100)
  }
  execute(g1)
  //逐行打印: 1
  //3
  //5
  //2
  //4
  //6
  //98
  //99
  //100
}