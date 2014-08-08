package rescala.lms.dsl

import org.scalatest._
import virtualization.lms.common.{CompileScala, RecursiveTransformer, ForwardTransformer}
import rescala.lms._
import rescala.{Var => REVar,Signal => RESignal, SignalSynt => RESignalSynt}
import java.io.{PrintWriter}
import scala.collection._
import rescala.lms.optimizations.FusionTransformers

import scala.util.matching._

class SigExprFusionSpec extends WordSpec with Matchers {
  "SigExprFusion" can {
    "replace signal expression with a single dep with map" in {
      val prog = new SigExprFusionProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val trans = FusionTransformers.sigApplyDepTransformer(prog)

      val transformed = { _: trans.IR.Rep[Unit] =>
        trans.reflectBlock(
          trans.transformBlock(
            prog.reifyEffects(
              prog.f(
                prog.fresh[Unit]))))
      }

      val res = prog.compile(transformed).apply()

      val (out1,out2) = (new java.io.StringWriter, new java.io.StringWriter)
      prog.codegen.emitSource(prog.f, "Untransformed", new java.io.PrintWriter(out1))
      prog.codegen.emitSource(transformed, "Transformed", new java.io.PrintWriter(out2))

      res.get should equal(43)

      println(out1.toString)
      println("-" * 50 + "\n")
      println(out2.toString)
    }
  }
}

trait SigExprFusionProg extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    val s0 = Var(unit(42))

    val s1 = Signal(s0) { s: Rep[RESignalSynt[Int]] => s0(s) + 1}

    s1
  }
}
