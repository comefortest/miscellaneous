package xyz.comefortest.miscellaneous.control

//an enhanced "break" in scala

import scala.util.control.NoStackTrace

private final case class MyBreakControl[T]( final val r: Option[T], final val obj: Breakable[T]) extends Exception;
private final case class MyNothingBreakControl( final val obj: Breakable[_]) extends Exception //with NoStackTrace;
private final class OverallNothingBreakControl extends Exception;

class Breakable[T] {
  def break(r: Option[T]) = { throw new MyBreakControl(r, this) }
  def break = throw new MyNothingBreakControl(this);
  def apply(f: => Option[T]): Option[T] = {
    try {
      f
    } catch {
      case MyBreakControl(r: Option[T], obj) if obj == this => r;
      case MyNothingBreakControl(obj) if obj == this => None;
      case _: OverallNothingBreakControl => None;
    }
  }
}
object Breakable {
  def apply[T]() = { new Breakable[T] }
  def break = { throw new OverallNothingBreakControl; }
  def breakable(f: => Unit): Unit = {
    try {
      f
    } catch {
      case _: OverallNothingBreakControl => None;
    }
  }
}