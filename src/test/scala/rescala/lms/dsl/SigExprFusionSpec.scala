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

      val result = prog.compile({_: trans.IR.Rep[Unit] => trans.reflectBlock(trans.transformBlock(prog.reifyEffects(prog.f(prog.fresh[Unit]))))}).apply( () )

      result.get should equal(43)
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
