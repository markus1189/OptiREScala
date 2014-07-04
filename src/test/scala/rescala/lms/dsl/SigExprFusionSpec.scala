package rescala.lms.dsl

import org.scalatest._
import virtualization.lms.common.{CompileScala, ForwardTransformer}
import rescala.lms._
import rescala.{Var => REVar,Signal => RESignal, SignalSynt => RESignalSynt}
import java.io.{PrintWriter}

import scala.util.matching._

class SigExprFusionSpec extends WordSpec with Matchers {
  "SigExprFusion" can {
    "replace signal expression with a single dep with map" in {
      val prog = new SigExprFusionProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val blk = prog.reifyEffects(prog.f(prog.fresh[Unit]))
      val codegen = prog.codegen

      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(blk)
      }

      val trans = new ForwardTransformer {
        val IR: prog.type = prog
        import IR._
        override def transformStm(stm: Stm) = stm match {
          case TP(s,SigApplyDep(s1,s2)) => s1  // x1(x2) --> x1
          case _ => super.transformStm(stm)
        }
      }

      val transformed = trans.transformBlock(blk)

      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(transformed)
      }
    }
  }
}

trait SigExprFusionProg extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    val s0 = Var(42)

    val s1 = Signal(s0) { s: Rep[RESignalSynt[Int]] => s0(s) + 1}

    s1
  }
}
