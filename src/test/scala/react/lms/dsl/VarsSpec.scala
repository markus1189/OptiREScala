package lms.react.dsl

import org.scalatest._
import virtualization.lms.common.CompileScala
import react.lms._

class VarsSpec extends WordSpec with Matchers {
  "A OptiREScala Var" can {
    "be created as normally" in {
      val prog = new CreateVarProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val res = prog.compile(prog.f).apply( () )
      res.getVal should equal(42)
    }
  }
}

trait CreateVarProg extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    Var(42)
  }
}
