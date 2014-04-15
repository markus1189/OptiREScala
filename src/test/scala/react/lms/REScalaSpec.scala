package react.lms

import org.scalatest._

import react._

class REScalaSpec extends WordSpec with Matchers {
  "Signals" can {
    "be created and queried" in {
      val s:  SignalSynt[Int] = SignalSynt[Int](List()) { _ => 42 }
      s.getVal should equal (42)
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
