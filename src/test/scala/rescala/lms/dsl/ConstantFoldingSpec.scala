package rescala.lms.dsl

import org.scalatest._
import virtualization.lms.common.CompileScala
import rescala.lms._
import rescala.lms.optimizations._
import rescala.{Var => REVar,Signal => RESignal, SignalSynt => RESignalSynt, DepHolder, StaticSignal}

import scala.util.matching._

class ConstantFoldingSpec extends WordSpec with Matchers {
  "ConstantFolding" should {
    "fold static Signals" in {
      val prog = new StaticSignalFoldProg with ReactiveDSLExp with ConstantFolding with CompileScala { self =>
        override val codegen = new ReactiveDSLGen with ScalaGenConstantFolding {
          val IR: self.type = self
        }
      }

      val s = prog.compile(prog.f).apply( () )

      s.get should equal(42)

      val out = new java.io.StringWriter();
      prog.codegen.emitSource(prog.f, "F", new java.io.PrintWriter(out))
      out.toString should include regex "rescala.Constant" // Constant created
    }
  }
}

trait StaticSignalFoldProg extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    StaticSignal() { 42 }
  }
}
