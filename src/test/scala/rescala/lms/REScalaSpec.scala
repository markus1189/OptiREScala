package rescala.lms

import org.scalatest._

import rescala._

class REScalaSpec extends WordSpec with Matchers {
  "Signals" can {
    "be created and queried" in {
      val s:  Signal[Int] = SignalSynt[Int](List()) { _ => 42 }
      s.get should equal (42)
    }

    "update automatically" in {
      val v1 = VarSynt[Int](20)
      val v2 = VarSynt[Int](22)

      val s = SignalSynt[Int](List(v1,v2)) { s: SignalSynt[Int] => v1(s) + v2(s) }
      s.get should equal(42)

      v1.set(0)
      s.get should equal(22)
    }
  }

  "Vars" can {
    "be created" in {
      val v = VarSynt[Int](42)
      v.get should equal (42)
    }

    "be changed" in {
        val v = VarSynt[Int](42)
      v.get should equal (42)
      v.set(21)
      v.get should equal(21)
    }
  }
}
