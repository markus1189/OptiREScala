package rescala.lms.optimizations

import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenEffect}
import rescala.lms.syntaxops._
import rescala.{DepHolder, Signal => RESignal, SignalSynt => RESignalSynt}

trait ConstantFolding extends EffectExp with SignalOps with SignalSyntax {

  private def lambdaHasOnlyConstants[A:Manifest](f: Exp[RESignalSynt[A]] => Exp[A]): Boolean = {

    val ff = fun(f)
    syms(ff).flatMap(findDefinition(_)).headOption match {
      case Some(TP(_,Lambda(g,x1,y1))) =>
        val symsInLambda = syms(y1).flatMap(findDefinition(_))
        symsInLambda.forall {
          case TP(_,Reify(_,_,deps)) => onlyConstants(deps)
          case _ => true
        }
      case _ => throw new Error("Should not happen because we create a Lambda above")
    }
  }

  // Workhorse of constant folding, if this is true we can inline
  private def onlyConstants: Seq[Exp[Any]] => Boolean = {

    // 1) we are only interested in Sym
    def filterForSyms[A]: Seq[Exp[A]] => Seq[Sym[A]] =
      _.foldLeft(Seq[Sym[A]]()) {
        case (acc,x@Sym(_)) => acc :+ x
        case (acc,_) => acc
      }

    // 2) find the definitions for the Sym
    def retrieveDefinition[A]: Seq[Sym[A]] => Seq[Def[Any]] =
      _.map(findDefinition(_)).flatMap {
        case Some(TP(_,rhs)) => Some(rhs)
        case _ => None
      }

    // 3) check if it is a constant
    def allAreConstant: Seq[Def[Any]] => Boolean = _.forall {
      case ConstantCreation(_) => true
      case _ => false
    }

    allAreConstant compose retrieveDefinition compose filterForSyms
  }

 override def sig_ops_newStaticSignal[A:Manifest](
    dhs: Seq[Exp[DepHolder]], f: => Exp[A]): Exp[RESignal[A]] = {

    if (dhs.isEmpty || onlyConstants(dhs)) {
      ConstantCreation(reifyEffects(f))
    } else {
      StaticSignalCreation(dhs, reifyEffects(f))
    }
 }

  override def sig_ops_newSignal[A:Manifest](deps: Seq[Exp[DepHolder]],
    expr: Exp[RESignalSynt[A]] => Exp[A]): Exp[RESignalSynt[A]] = {

    val constantLambda: Boolean = lambdaHasOnlyConstants(expr)

    val regularSignal = SignalCreation(deps,fun(expr))

    val a: Exp[A] = expr(regularSignal)

    if (deps.isEmpty || (onlyConstants(deps) && constantLambda)) {
        ConstantCreation(Block(a))
    } else {
      SignalCreation(deps,fun(expr))
    }
  }

  case class ConstantCreation[A:Manifest](body: Block[A]) extends Def[RESignalSynt[A]]
  case class ConstantAccess[A:Manifest](body: Block[A]) extends Def[A]

  override def sig_ops_get[A:Manifest](s: Exp[RESignal[A]]): Exp[A] = s match {
    case Def(ConstantCreation(bdy)) => ConstantAccess(bdy)
    case _ => super.sig_ops_get(s)
  }

  override def sig_ops_apply[A:Manifest](s: Exp[RESignal[A]]): Exp[A] = s match {
    case Def(ConstantCreation(bdy)) => ConstantAccess(bdy)
    case _ => super.sig_ops_apply(s)
  }

  override def sig_ops_apply_dep[A:Manifest](sig: Exp[RESignal[A]], depSig: Exp[RESignalSynt[_]]): Exp[A] = sig match {
    case Def(ConstantCreation(bdy)) => ConstantAccess(bdy)
    case _ => super.sig_ops_apply_dep(sig,depSig)
  }
}

trait ScalaGenConstantFolding extends ScalaGenReactiveBase with ScalaGenEffect {
  val IR: ConstantFolding
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
    case ConstantAccess(f) => emitValDef(sym, quote(getBlockResult(f)))
    case ConstantCreation(f) => emitValDef(sym,
      rescalaPkg + "Constant{ // tag:constant-creation")
        emitBlock(f)
        stream.println(quote(getBlockResult(f)))
      stream.println("}")
    case _ => super.emitNode(sym,node)
  }
}
