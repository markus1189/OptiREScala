package react.lms.syntaxops

import react.{Var => REVar,Signal => RESignal}
import scala.language.implicitConversions

import scala.virtualization.lms.common.{Base, ScalaGenBase, EffectExp}

/** Define the available operations for the DSL users */
trait VarSyntax extends Base {
  object Var {
    def apply[A:Manifest](v: Rep[A]): Rep[REVar[A]] = var_ops_newVar(v)
  }

  def var_ops_newVar[A:Manifest](v: Rep[A]): Rep[REVar[A]]

  implicit def toVarOps[A:Manifest](v: Rep[REVar[A]]) = new VarOps(v)

  // After implicit conversion, this class receives the methods called
  // on a DSL version of a Var, ops defined here are available to
  // end-users.

  // Defining an op here is delegated to an abstract method, whose
  // implementation will be in the corresponding -Ops trait.
  class VarOps[A:Manifest](v: Rep[REVar[A]]) {
    def setVal(x: Rep[A]): Rep[Unit] = var_ops_setVal(v,x)
    def update(x: Rep[A]): Rep[Unit] = var_ops_update(v,x)
    def getValue: Rep[A] = var_ops_getValue(v)
    def getVal: Rep[A] = getValue
    def apply(): Rep[A] = var_ops_apply(v)
    def apply(s: Rep[RESignal[_]]): Rep[A] = var_ops_apply_sig(v,s)
    def toSignal: Rep[RESignal[A]] = var_ops_toSignal(v)
  }

  def var_ops_setVal[A:Manifest](v: Rep[REVar[A]], x: Rep[A]): Rep[Unit]
  def var_ops_update[A:Manifest](v: Rep[REVar[A]], x: Rep[A]): Rep[Unit]
  def var_ops_getValue[A:Manifest](v: Rep[REVar[A]]): Rep[A]
  def var_ops_apply[A:Manifest](v: Rep[REVar[A]]): Rep[A]
  def var_ops_apply_sig[A:Manifest](v: Rep[REVar[A]], s: Rep[RESignal[_]]): Rep[A]
  def var_ops_toSignal[A:Manifest](v: Rep[REVar[A]]): Rep[RESignal[A]]
}

trait VarOps extends BaseExp {
}

trait ScalaGenVars extends ScalaGenBase {
}
