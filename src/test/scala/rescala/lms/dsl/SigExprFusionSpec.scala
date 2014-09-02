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

      val trans = FusionTransformers.sigExprToMap(prog)

      val arg = prog.fresh[Unit]
      val blk = prog.reifyEffects(prog.f(arg))

      val out = new java.io.StringWriter
      prog.codegen.withStream(new PrintWriter(out)) {
        prog.codegen.emitBlock(blk)
      }

      val transformedProg = { _: trans.IR.Rep[Unit] =>
        trans.reflectBlock(trans.transformBlock(blk))
      }

      prog.codegen.emitSource(transformedProg, "Transformed", new java.io.PrintWriter(out))

      val result = prog.compile(transformedProg).apply()
      result.get should equal(43)

      out.toString.lines.toList.filter(_.contains("MappedSignal")) should have length(1)
    }
  }
}

trait SigExprFusionProg extends ReactiveDSL {
  def f(x : Rep[Unit]): Rep[RESignal[Int]] = {
    val s0 = Signal() { s: Rep[RESignalSynt[Int]] => unit(42) }

    val s1 = Signal(s0) { s: Rep[RESignalSynt[Int]] => s0(s) + 1}

    s1
  }
}
