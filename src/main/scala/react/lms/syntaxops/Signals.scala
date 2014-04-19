package react.lms.syntaxops

import scala.language.implicitConversions

// avoid name clashes with object factory methods
import react.{Signal => RESignal}
import scala.virtualization.lms.common.{Base, BaseExp}

trait SignalSyntax extends Base {
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

trait SignalOps extends BaseExp {
  this: SignalSyntax =>

  override def sig_ops_getValue[A:Manifest](s: Exp[RESignal[A]]): Exp[A] = SigGetValue(s)
  case class SigGetValue[A:Manifest](s: Exp[RESignal[A]]) extends Def[A]

  override def sig_ops_apply[A:Manifest](s: Exp[RESignal[A]]): Exp[A] = SigApply(s)
  case class SigApply[A:Manifest](s: Exp[RESignal[A]]) extends Def[A]

  override def sig_ops_apply_dep[A:Manifest](sig: Exp[RESignal[A]], depSig: Exp[RESignal[_]]): Exp[A]
  case class SigApplyDep[A:Manifest](sig: Exp[RESignal[A]], depSig: Exp[RESignal[A]]) extends Def[A]
}

trait ScalaGenSignals extends ScalaGenReactiveBase {
  val IR: SignalOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case _ => super.emitNode(sym,node)
  }
}
