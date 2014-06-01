package rescala.lms.dsl

import org.scalatest._
import virtualization.lms.common.CompileScala
import rescala.lms._
import rescala._

class SignalsSpec extends WordSpec with Matchers {
  "A OptiREScala Signal" can {
    "be created as normally" in {
      val prog = new CreateSigProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val res = prog.compile(prog.f).apply( () )
      res.get should equal(42)
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

    "be mapped" in {
      val prog = new MapSignalProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val res = prog.compile(prog.f).apply( () )
      res.get should equal(42)
    }
  }
}

// Create a Signal
trait CreateSigProg extends ReactiveDSL {
  def f(x : Rep[Unit]) = Signal(List()){x: Rep[Signal[Int]] => 42}
}

// Create a Signal and call get on in
trait GetFromSigProg extends ReactiveDSL {
  def f(x:Rep[Unit]) = {
    val s = Signal(List()){x: Rep[Signal[String]] => "1337" }
    s.get
  }
}

// Create a Signal and call apply on it
trait ApplySigProg extends ReactiveDSL {
  def f(x:Rep[Unit]) = {
    val s = Signal(List()){x: Rep[Signal[Double]] => 3.141592 }
    s()
  }
}

// Map over a Signal
trait MapSignalProg extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    val v = Var(39)
    def inc(i: Rep[Int]): Rep[Int] = i + 1

    Signal(List(v)) { s: Rep[Signal[Int]] => v() }.
      map(inc).
      map(inc).
      map(inc)
  }
}
