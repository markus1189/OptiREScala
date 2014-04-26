package react.lms

import org.scalatest._

import react._

class REScalaSpec extends WordSpec with Matchers {
  "Signals" can {
    "be created and queried" in {
      val s:  SignalSynt[Int] = SignalSynt[Int](List()) { _ => 42 }
      s.getVal should equal (42)
    }

    "update automatically" in {
      val v1 = VarSynt[Int](20)
      val v2 = VarSynt[Int](22)

      val s = SignalSynt[Int](List(v1,v2)) { s: SignalSynt[Int] => v1(s) + v2(s) }
      s.getVal should equal(42)

      v1.setVal(0)
      s.getVal should equal(22)
    }
  }

  "Vars" can {
    "be created" in {
      val v = VarSynt[Int](42)
      v.getVal should equal (42)
    }

    "be changed" in {
        val v = VarSynt[Int](42)
      v.getVal should equal (42)
      v.setVal(21)
      v.getVal should equal(21)
    }
  }
}
