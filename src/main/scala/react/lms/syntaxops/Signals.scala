package react.lms.syntaxops

import scala.language.implicitConversions

// avoid name clashes with object factory methods
import react.{Signal => RESignal, DepHolder}
import scala.virtualization.lms.common.{Base, BaseExp, FunctionsExp, ScalaGenFunctions}

trait SignalSyntax extends Base {
  object Signal {
    def apply[A:Manifest](
      deps: Rep[List[DepHolder]]
    )(
      expr: Rep[RESignal[A]] => Rep[A]
    ) = sig_ops_newSignal(deps, expr)
  }

  def sig_ops_newSignal[A:Manifest](deps: Rep[List[DepHolder]],
    expr: Rep[RESignal[A]] => Rep[A]): Rep[RESignal[A]]

  implicit def toSignalOps[A:Manifest](s: Rep[RESignal[A]]) = SignalOps(s)

  case class SignalOps[A:Manifest](sig: Rep[RESignal[A]]) {
    def getValue: Rep[A] = sig_ops_getValue(sig)
    def getVal: Rep[A] = getValue
    def apply(): Rep[A] = sig_ops_apply(sig)
    def apply(depSig: Rep[RESignal[_]]): Rep[A] = sig_ops_apply_dep(sig,depSig)
  }

  def sig_ops_getValue[A:Manifest](s: Rep[RESignal[A]]): Rep[A]
  def sig_ops_apply[A:Manifest](s: Rep[RESignal[A]]): Rep[A]
  def sig_ops_apply_dep[A:Manifest](sig: Rep[RESignal[A]], depSig: Rep[RESignal[_]]): Rep[A]
}

trait SignalOps extends BaseExp with FunctionsExp {
  this: SignalSyntax =>

  def sig_ops_newSignal[A:Manifest](deps: Exp[List[DepHolder]],
    expr: Exp[RESignal[A]] => Exp[A]): Exp[RESignal[A]] = SignalCreation(deps,fun(expr))

  case class SignalCreation[A:Manifest](deps: Exp[List[DepHolder]],
    expr: Exp[RESignal[A] => A]) extends Def[RESignal[A]] {
    val t = manifest[A]
  }

  override def sig_ops_getValue[A:Manifest](s: Exp[RESignal[A]]): Exp[A] = SigGetValue(s)
  case class SigGetValue[A:Manifest](s: Exp[RESignal[A]]) extends Def[A]

  override def sig_ops_apply[A:Manifest](s: Exp[RESignal[A]]): Exp[A] = SigApply(s)
  case class SigApply[A:Manifest](s: Exp[RESignal[A]]) extends Def[A]

  override def sig_ops_apply_dep[A:Manifest](sig: Exp[RESignal[A]], depSig: Exp[RESignal[_]]): Exp[A] = SigApplyDep(sig,depSig)
  case class SigApplyDep[A:Manifest](sig: Exp[RESignal[A]], depSig: Exp[RESignal[_]]) extends Def[A]
}

trait ScalaGenSignals extends ScalaGenReactiveBase with ScalaGenFunctions {
  val IR: SignalOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case s@SignalCreation(deps,expr) => emitSignalCreation(s,sym)
    case SigGetValue(s) => emitValDef(sym, quote(s) + ".getValue")
    case SigApply(s) => emitValDef(sym, quote(s) + "()")
    case SigApplyDep(s,dep) => ???
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
