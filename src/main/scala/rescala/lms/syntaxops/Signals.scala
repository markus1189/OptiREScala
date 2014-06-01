package rescala.lms.syntaxops

import scala.language.implicitConversions

// avoid name clashes with object factory methods
import rescala.{DepHolder, SignalSynt => RESignalSynt, Signal => RESignal}
import scala.virtualization.lms.common.{Base, BaseExp, FunctionsExp, ScalaGenFunctions, EffectExp}

trait SignalSyntax extends Base {
  object Signal {
    def apply[A:Manifest](
      deps: Rep[List[DepHolder]]
    )(
      expr: Rep[RESignalSynt[A]] => Rep[A]
    ) = sig_ops_newSignal(deps, expr)
  }

  def sig_ops_newSignal[A:Manifest](deps: Rep[List[DepHolder]],
    expr: Rep[RESignalSynt[A]] => Rep[A]): Rep[RESignalSynt[A]]

  implicit def toSignalOps[A:Manifest](s: Rep[RESignal[A]]) = SignalOps(s)

  case class SignalOps[A:Manifest](sig: Rep[RESignal[A]]) {
    def get: Rep[A] = sig_ops_get(sig)
    def apply(): Rep[A] = sig_ops_apply(sig)
    def apply(depSig: Rep[RESignalSynt[_]]): Rep[A] = sig_ops_apply_dep(sig,depSig)
  }

  def sig_ops_get[A:Manifest](s: Rep[RESignal[A]]): Rep[A]
  def sig_ops_apply[A:Manifest](s: Rep[RESignal[A]]): Rep[A]
  def sig_ops_apply_dep[A:Manifest](sig: Rep[RESignal[A]], depSig: Rep[RESignalSynt[_]]): Rep[A]
}

trait SignalOps extends BaseExp with FunctionsExp with EffectExp {
  this: SignalSyntax =>

  def sig_ops_newSignal[A:Manifest](deps: Exp[List[DepHolder]],
    expr: Exp[RESignalSynt[A]] => Exp[A]): Exp[RESignalSynt[A]] = SignalCreation(deps,fun(expr))

  case class SignalCreation[A:Manifest](deps: Exp[List[DepHolder]],
    expr: Exp[RESignalSynt[A] => A]) extends Def[RESignalSynt[A]] {
    val t = manifest[A]
  }

  override def sig_ops_get[A:Manifest](s: Exp[RESignal[A]]): Exp[A] =
    reflectMutable(SigGetValue(s))
  case class SigGetValue[A:Manifest](s: Exp[RESignal[A]]) extends Def[A]

  override def sig_ops_apply[A:Manifest](s: Exp[RESignal[A]]): Exp[A] =
    reflectMutable(SigApply(s))
  case class SigApply[A:Manifest](s: Exp[RESignal[A]]) extends Def[A]

  override def sig_ops_apply_dep[A:Manifest](sig: Exp[RESignal[A]], depSig: Exp[RESignalSynt[_]]): Exp[A] =
    reflectMutable(SigApplyDep(sig,depSig))
  case class SigApplyDep[A:Manifest](sig: Exp[RESignal[A]], depSig: Exp[RESignalSynt[_]]) extends Def[A]
}

trait ScalaGenSignals extends ScalaGenReactiveBase with ScalaGenFunctions {
  val IR: SignalOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case s@SignalCreation(deps,expr) => emitSignalCreation(s,sym)
    case SigGetValue(s) => emitValDef(sym, quote(s) + ".get")
    case SigApply(s) => emitValDef(sym, quote(s) + "()")
    case SigApplyDep(s,dep) => emitValDef(sym, quote(s) + "(" + quote(dep) + ")")
    case _ => super.emitNode(sym,node)
  }

  def emitSignalCreation[A:Manifest](sigNode: SignalCreation[A], sym: Sym[Any]): Unit = {
    val SignalCreation(deps,expr) = sigNode

    val className = "SignalSynt[" + sigNode.t + "]" // use manifest for type ascription
    val quotedDeps = "(" + quote(deps) + ")"
    val quotedExpr = "{" + quote(expr) + "}"

    emitValDef(sym, rescalaPkg + className + quotedDeps + quotedExpr )
  }
}