package react.lms.syntaxops

import scala.language.implicitConversions

// avoid name clashes with object factory methods
import react.{Signal => RESignal}
import scala.virtualization.lms.common.{Base, EffectExp}

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
}
