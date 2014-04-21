package lms.react.dsl

import org.scalatest._
import virtualization.lms.common.CompileScala
import react.lms._
import react._

class SignalsSpec extends WordSpec with Matchers {
  "A OptiREScala Signal" can {
    "be created as normally" in {
      val prog = new CreateSigProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val res = prog.compile(prog.f).apply( () )
      res.getVal should equal(42)
    }

    "be asked for its value" in {
      val prog = new GetFromSigProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val res = prog.compile(prog.f).apply( () )
      res should equal("1337")
    }

    "be asked for its value using the apply method" in {
      val prog = new ApplySigProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val res = prog.compile(prog.f).apply( () )
      res should equal(3.141592)
    }
  }
}

trait CreateSigProg extends ReactiveDSL {
  def f(x : Rep[Unit]) = Signal(List()){x: Rep[Signal[Int]] => 42}
}

trait GetFromSigProg extends ReactiveDSL {
  def f(x:Rep[Unit]) = {
    val s = Signal(List()){x: Rep[Signal[String]] => "1337" }
    s.getVal
  }
}

trait ApplySigProg extends ReactiveDSL {
  def f(x:Rep[Unit]) = {
    val s = Signal(List()){x: Rep[Signal[Double]] => 3.141592 }
    s()
  }
}
