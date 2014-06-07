package rescala.lms.optimizations

import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenEffect}
import rescala.lms.syntaxops._
import rescala.{DepHolder, Signal => RESignal}

trait ConstantFolding extends EffectExp {
  self: SignalOps =>

  // Workhorse of constant folding, if this is true we can inline
  private def onlyConstants: Seq[Exp[DepHolder]] => Boolean = {

    // We are only interested in Sym
    def filterForSyms[A]: Seq[Exp[A]] => Seq[Sym[A]] =
      _.foldLeft(Seq[Sym[A]]()) {
        case (acc,x@Sym(_)) => acc :+ x
        case (acc,_) => acc
      }

    // Find the definitions for the Sym
    def retrieveDefinition[A]: Seq[Sym[A]] => Seq[Def[Any]] =
      _.map(findDefinition(_)).flatMap {
        case Some(TP(_,rhs)) => Some(rhs)
        case _ => None
      }

    // Check if it is a constant
    def allAreConstant: Seq[Def[Any]] => Boolean = _.forall {
      case ConstantCreation(_) => true
      case _ => false
    }

    // Compose the helpers above into a single function
    allAreConstant compose retrieveDefinition compose filterForSyms
  }

  // override def new_behavior[A:Manifest](
  //   dhs: Seq[Exp[DepHolder]], f: => Exp[A]): Exp[RESignal[A]] = {

  //   if (dhs.isEmpty || onlyConstants(dhs)) {
  //     ConstantCreation(reifyEffects(f))
  //   } else {
  //     SignalCreation(dhs, reifyEffects(f))
  //   }
  // }

  case class ConstantCreation[A:Manifest](body: Block[A]) extends Def[RESignal[A]]
  case class ConstantAccess[A:Manifest](body: Block[A]) extends Def[A]

  // override def dep_holder_access[A:Manifest](dh: Exp[AccessableDepHolder[A]]): Exp[A] = dh match {
  //   case Def(ConstantCreation(x)) => ConstantAccess(x)
  //   case _ => super.dep_holder_access(dh)
  // }

}

// trait ScalaGenConstantFolding extends ScalaGenReactiveBase with ScalaGenEffect {
//   val IR: ConstantFolding
//   import IR._

//   override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
//     case ConstantAccess(f) => emitValDef(sym, quote(getBlockResult(f)))
//     /* Unfold the stored block inside of a Constant expression */
//     case ConstantCreation(f) => emitValDef(sym,
//       simpleReactivePkg + "Constant {")
//         emitBlock(f)
//         stream.println(quote(getBlockResult(f)) + "\n")
//       stream.println("}")
//     case _ => super.emitNode(sym,node)
//   }
// }
