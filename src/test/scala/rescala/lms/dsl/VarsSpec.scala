package rescala.lms.dsl

import org.scalatest._
import virtualization.lms.common.CompileScala
import rescala.lms._
import rescala._

class VarsSpec extends WordSpec with Matchers {
  "A OptiREScala Var" can {
    "be created as normally" in {
      val prog = new CreateVarProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val res = prog.compile(prog.f).apply( () )
      res.get should equal(42)
    }

    "be asked for its value" in {
      val prog = new GetFromVarProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val res = prog.compile(prog.f).apply( () )
      res should equal(42)
    }

    "be asked for its value using the apply method" in {
      val prog = new ApplyVarProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val res = prog.compile(prog.f).apply( () )
      res should equal(1337)
    }

    "be changed to another value" in {
      val prog = new SetVarProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val res = prog.compile(prog.f).apply( () )
      res.get should equal(21)
    }
  }
}

trait CreateVarProg extends ReactiveDSL {
  def f(x : Rep[Unit]) = Var(42)
}

trait GetFromVarProg extends ReactiveDSL {
  def f(x:Rep[Unit]) = {
    val v = Var(42)
    v.get
  }
}

trait SetVarProg extends ReactiveDSL {
  def f(x:Rep[Unit]) = {
    val v = Var(42)
    v.set(21)
    v
  }
}

trait ApplyVarProg extends ReactiveDSL {
  def f(x:Rep[Unit]) = {
    val v = Var(1337)
    v()
  }
}
