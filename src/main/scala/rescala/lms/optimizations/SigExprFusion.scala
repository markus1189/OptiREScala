package rescala.lms.optimizations

import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenEffect, FunctionsExp}
import scala.virtualization.lms.internal.Expressions
import language.implicitConversions
import rescala.lms.syntaxops._

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
