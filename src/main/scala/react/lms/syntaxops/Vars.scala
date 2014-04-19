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

/** Implement the abstract syntax methods, typically introducing new
    DSL 'nodes' that will later be used in the corresponding ScalaGen-
    trait*/
trait VarOps extends EffectExp {
  this: VarSyntax =>

  override def var_ops_newVar[A:Manifest](v: Exp[A]): Exp[REVar[A]] = VarCreation(v)
  case class VarCreation[A:Manifest](
    value: Exp[A]) extends Def[REVar[A]]

  override def var_ops_setVal[A:Manifest](v: Exp[REVar[A]], x: Exp[A]): Exp[Unit] = reflectEffect(SetValue(v,x))
  case class SetValue[A:Manifest](
    varToSet: Exp[REVar[A]], newValue: Exp[A]) extends Def[Unit]

  override def var_ops_update[A:Manifest](v: Exp[REVar[A]], x: Exp[A]): Exp[Unit] = UpdateValue(v,x)
  case class UpdateValue[A:Manifest](
    varToUpdate: Exp[REVar[A]], newValue: Exp[A]) extends Def[Unit]

  override def var_ops_getValue[A:Manifest](v: Exp[REVar[A]]): Exp[A] = GetValue(v)
  case class GetValue[A:Manifest](
    varToAccess: Exp[REVar[A]]) extends Def[A]

  override def var_ops_apply[A:Manifest](v: Exp[REVar[A]]): Exp[A] = VarApply(v)
  case class VarApply[A:Manifest](
    varToApply: Exp[REVar[A]]) extends Def[A]

  override def var_ops_apply_sig[A:Manifest](v: Exp[REVar[A]], s: Exp[RESignal[_]]): Exp[A] = VarApplyWithDep(v,s)
  case class VarApplyWithDep[A:Manifest](
    varToApply: Exp[REVar[A]], dep: Exp[RESignal[_]]) extends Def[A]

  override def var_ops_toSignal[A:Manifest](v: Exp[REVar[A]]): Exp[RESignal[A]] = VarToSignal(v)
  case class VarToSignal[A:Manifest](varToConvert: Exp[REVar[A]]) extends Def[RESignal[A]]
}

trait ScalaGenVars extends ScalaGenReactiveBase {
  val IR: VarOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case VarCreation(v) => emitValDef(sym, rescalaPkg + "Var(" + quote(v) + ")")
    case GetValue(v) => emitValDef(sym, quote(v) + ".getValue")
    case SetValue(v,x) => emitValDef(sym, quote(v) + ".setVal(" + quote(x) + ")")
    case _ => super.emitNode(sym,node)
  }
}
