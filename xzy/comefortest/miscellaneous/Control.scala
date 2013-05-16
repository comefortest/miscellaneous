package xyz.comefortest.miscellaneous.control

//an enhanced "break" for scala

import scala.util.control.NoStackTrace

private final case class MyBreakControl0[T](val r: Option[T], val obj: Breakable[T]) extends Exception //with NoStackTrace
class Breakable[T] {
  def break(r: Option[T]): Nothing = { throw new MyBreakControl0(r, this) }
  def break: Nothing = break(None)
  def apply(f: => Option[T]): Option[T] = {
    try {
      f
    } catch {
      case MyBreakControl0(r: Option[T], obj) if obj == this => r
    }
  }
  def apply(f: => Unit): Unit = {
    try {
      f
    } catch {
      case MyBreakControl0(r: Option[T], obj) if obj == this => {}
    }
  }
}
object Breakable {
  def apply[T]() = { new Breakable[T] }

  private object DummyBreakControl extends Exception with NoStackTrace
  def break: Nothing = { throw DummyBreakControl }
  def breakable(f: => Unit): Unit = {
    try {
      f
    } catch {
      case x: MyBreakControl1[_] => {}
      case DummyBreakControl => {}
    }
  }

  private final case class MyBreakControl1[T](val r: Option[T]) extends Exception //with NoStackTrace
  def break[T](r: Option[T]): Option[T] = { throw new MyBreakControl1(r) }
  def breakable[T](f: => Option[T]): Option[T] = {
    try {
      f
    } catch {
      case MyBreakControl1(r: Option[T]) => r
      case DummyBreakControl => None
    }
  }
}