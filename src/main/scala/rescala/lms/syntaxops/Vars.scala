package rescala.lms.syntaxops

import rescala.{Var => REVar,Signal => RESignal}

import scala.language.implicitConversions

import scala.virtualization.lms.common.{Base, EffectExp, BaseExp}
import scala.reflect.SourceContext

/** Define the available operations for the DSL users */
trait VarSyntax extends Base {
  object Var {
    def apply[A:Manifest](v: Rep[A]): Rep[REVar[A]] = var_ops_newVar(v)
  }

  def var_ops_newVar[A:Manifest](v: Rep[A]): Rep[REVar[A]]

  implicit def toVarOps[A:Manifest](v: Rep[REVar[A]]) = VarOps(v)

  // After implicit conversion, this class receives the methods called
  // on a DSL version of a Var, ops defined here are available to
  // end-users.

  // Defining an op here is delegated to an abstract method, whose
  // implementation will be in the corresponding -Ops trait.
  case class VarOps[A:Manifest](v: Rep[REVar[A]]) {
    def set(x: Rep[A]): Rep[Unit] = var_ops_set(v,x)
    def update(x: Rep[A]): Rep[Unit] = var_ops_update(v,x)
  }

  def var_ops_set[A:Manifest](v: Rep[REVar[A]], x: Rep[A]): Rep[Unit]
  def var_ops_update[A:Manifest](v: Rep[REVar[A]], x: Rep[A]): Rep[Unit]
}

/** Implement the abstract syntax methods, typically introducing new
    DSL 'nodes' that will later be used in the corresponding ScalaGen-
    trait*/
trait VarOps extends EffectExp {
  this: VarSyntax with BaseExp =>

  override def var_ops_newVar[A:Manifest](v: Exp[A]): Exp[REVar[A]] = VarCreation(v)
  case class VarCreation[A:Manifest](
    value: Exp[A]) extends Def[REVar[A]] {
    val t = manifest[A]
  }

  override def var_ops_set[A:Manifest](v: Exp[REVar[A]], x: Exp[A]): Exp[Unit] = reflectEffect(SetValue(v,x))
  case class SetValue[A:Manifest](
    varToSet: Exp[REVar[A]], newValue: Exp[A]) extends Def[Unit] {
    val t = manifest[A]
  }

  override def var_ops_update[A:Manifest](v: Exp[REVar[A]], x: Exp[A]): Exp[Unit] = UpdateValue(v,x)
  case class UpdateValue[A:Manifest](
    varToUpdate: Exp[REVar[A]], newValue: Exp[A]) extends Def[Unit] {
    val t = manifest[A]
  }

  override def mirror[A:Manifest](
    e: Def[A],
    f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
      case VarCreation(x) => var_ops_newVar(f(x))
      case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenVars extends ScalaGenReactiveBase {
  val IR: VarOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case n@VarCreation(v) => emitValDef(sym, rescalaPkg + "VarSynt[" + n.t + "](" + quote(v) + ")")
    case SetValue(v,x) => emitValDef(sym, quote(v) + ".set(" + quote(x) + ")")
    case UpdateValue(v,x) => emitValDef(sym, quote(v) + ".update(" + quote(x) + ")")
    case _ => super.emitNode(sym,node)
  }
}
