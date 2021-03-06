package rescala.lms.optimizations

import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenEffect, FunctionsExp}
import scala.virtualization.lms.internal.Expressions
import scala.reflect.SourceContext
import language.implicitConversions
import rescala.lms.syntaxops._

import rescala.{Signal => RESignal}

/** Introduce map equivalent method called fuseMap */
trait MapFusionSyntax extends Base {

 implicit def toMapFusionOps[A:Manifest](s: Rep[RESignal[A]]) = MapFusionOps(s)
  case class MapFusionOps[A:Manifest](s: Rep[RESignal[A]]) {
    def fuseMap[B:Manifest](f: Rep[A] => Rep[B]): Rep[RESignal[B]] =
      sig_ops_fused_map(s,f)
    def fuseMapRep[B:Manifest](f: Rep[A => B]): Rep[RESignal[B]] =
      sig_ops_fused_map_rep(s,f)
  }

  def sig_ops_fused_map[A:Manifest,B:Manifest](
    s: Rep[RESignal[A]],
    f: Rep[A] => Rep[B]
  ): Rep[RESignal[B]]

  def sig_ops_fused_map_rep[A:Manifest,B:Manifest](
    s: Rep[RESignal[A]],
    f: Rep[A => B]
  ): Rep[RESignal[B]]
}

/** Override the standard map function with the fused version. */

trait MapFusionOverride extends Base {
  this: SignalOps with MapFusionSyntax =>

  override def sig_ops_map[A:Manifest,B:Manifest](sig: Rep[RESignal[A]],
    f: Rep[A] => Rep[B]): Rep[RESignal[B]] = sig_ops_fused_map(sig,f)

  override def sig_ops_map_rep[A:Manifest, B:Manifest](sig: Rep[RESignal[A]],
    f: Rep[A => B]): Rep[RESignal[B]] = sig_ops_fused_map_rep(sig, f)
}

/** Implement the fused mapping by creating nested versions of
  * MappedSignal classes, where the outer holds a composed version
  * of all functions stored inside wrapped classes
  */
trait MapFusionOps extends MapFusionSyntax with FunctionsExp {
  this: SignalOps =>

  /** Staged function composition */
  case class FunctionComposition[A:Manifest,B:Manifest,C:Manifest](
    f: Exp[B => C],
    g: Exp[A => B]
  ) extends Def[A => C]

  /** Infix function composition on staged lambdas: l1.compose(l2) */
  def infix_compose[A:Manifest,B:Manifest,C:Manifest](
    f: Exp[B => C], g: Exp[A => B]): Exp[A => C] =
    FunctionComposition(f,g)

  override def sig_ops_fused_map_rep[A:Manifest,B:Manifest](sig: Exp[RESignal[A]],
    f: Exp[A => B]): Exp[RESignal[B]] = sig match {

    case Def(MappedSignal(unmappedSig,g)) => // Map called on a already mapped signal
      MappedSignal(unmappedSig, f.compose(g))

    case _ => // First map or previous call was not a map
      MappedSignal(sig, f)
  }

  override def sig_ops_fused_map[A:Manifest,B:Manifest](sig: Exp[RESignal[A]],
    f: Exp[A] => Exp[B]): Exp[RESignal[B]] = sig_ops_fused_map_rep(sig, doLambda(f))

  override def mirror[A:Manifest](e: Def[A], t: Transformer)(
    implicit pos: SourceContext): Exp[A] = (e match {
      case FunctionComposition(f,g) => toAtom(FunctionComposition(f,g.asInstanceOf[Exp[A => Any]]))
      case _ => super.mirror(e, t)
    }).asInstanceOf[Exp[A]]
}

/** Provide code generation for the function composition helper */
trait ScalaGenMapFusion extends ScalaGenReactiveBase {
  val IR: MapFusionOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit =  node match {
    case FunctionComposition(f,g) => emitValDef(sym,
      quote(g) + ".compose(" + quote(f) +")")
    case _ => super.emitNode(sym,node)
  }
}
