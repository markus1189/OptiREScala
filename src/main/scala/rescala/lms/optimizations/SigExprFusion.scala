package rescala.lms.optimizations

import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenEffect, FunctionsExp, ForwardTransformer, CompileScala}
import scala.virtualization.lms.internal.Expressions
import language.implicitConversions
import rescala.lms.syntaxops._
import rescala.lms._

import rescala.{Signal => RESignal, SignalSynt => RESignalSynt, DepHolder}

/** Fuse signal expressions */
trait SigExprFusionSyntax extends Base {
  object SignalF {
    def apply[A:Manifest](deps: Rep[DepHolder]*)(expr: Rep[RESignalSynt[A]] => Rep[A]) =
      sig_expr_fusion_newSignal(deps,expr)
  }

  def sig_expr_fusion_newSignal[A:Manifest](deps: Seq[Rep[DepHolder]],
    expr: Rep[RESignalSynt[A]] => Rep[A]): Rep[RESignalSynt[A]]
}

trait SigExprFusionOps extends SigExprFusionSyntax {
  this: SignalOps =>

  override def sig_expr_fusion_newSignal[A:Manifest](deps: Seq[Rep[DepHolder]],
    expr: Rep[RESignalSynt[A]] => Rep[A]): Rep[RESignalSynt[A]] = deps match {
    case Seq() => ??? /** TODO: no dependencies, simple subst? */
    case Seq(d) => ??? /** TODO: one dependency, try to inline? */
    case _ => sig_ops_newSignal(deps,expr)
  }
}

object FusionTransformers {

  // sig.apply(sig2) --> sig.apply()
  def sigApplyDepTransformer(prog: CompileScala with ReactiveDSLExp) = {
    new ForwardTransformer {
      val IR: prog.type = prog
      import IR._
      override def transformStm(stm: Stm) = {
        stm match {
          case TP(s,Reflect(a@SigApplyDep(s1,s2),t,u)) =>   // x1(x2) --> x1()
            sig_ops_apply(s1)(a.t)
          case _ => super.transformStm(stm)
        }}
    }
  }
}
