package lms.react.dsl

import org.scalatest._
import virtualization.lms.common.CompileScala
import react.lms._
import react.{VarSynt => REVar,SignalSynt => RESignal}

import scala.util.matching._

class InferenceSepc extends WordSpec with Matchers {
  "A OptiREScala Signal with inference" can {
    "infer dependencies of vars" in {
      val prog = new VarInferenceProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val s = prog.compile(prog.f).apply( () )

      // Check the result value
      s.getVal should equal(1+2+3)

      // Check result of code generation
      val out = new java.io.StringWriter();
      prog.codegen.emitSource(prog.f, "F", new java.io.PrintWriter(out))
      val p = new Regex("val x(\\d+) = List\\(x(\\d+),x(\\d+)\\)")
      out.toString should include regex p
    }
  }
}

trait VarInferenceProg extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    val dep1 = Var(1)
    val dep2 = Var(2)
    val dep3 = Var(3)

    ISignal { s: Rep[RESignal[Int]] => dep1(s) + dep2.getVal + dep3(s) }
  }
}
