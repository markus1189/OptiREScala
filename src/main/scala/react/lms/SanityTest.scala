package react.lms
import react._

object Main extends App {
  println("Hello world.")

  var a = 10
  val s: SignalSynt[Int] = SignalSynt[Int](List())(s=> 1 + 1 + a )

  println(s())
}
