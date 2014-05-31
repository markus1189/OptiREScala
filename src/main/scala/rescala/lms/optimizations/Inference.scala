package rescala.lms.optimizations

import scala.virtualization.lms.common.{Base, EffectExp, ListOps, FunctionBlocksExp}
import rescala.lms.syntaxops.{SignalSyntax, SignalOps, VarOps}
import rescala.{SignalSynt, DepHolder}

/** Extend Syntax with a new form ISignal { } that infers dependencies */
trait SignalInferenceSyntax extends Base {
  this: SignalSyntax =>

  object ISignal {
    def apply[A:Manifest](f: Rep[SignalSynt[A]] => Rep[A]) =
      new_inferred_signal(f)
  }

  def new_inferred_signal[A:Manifest](
    f: Rep[SignalSynt[A]] => Rep[A]): Rep[SignalSynt[A]]
}

trait SignalInferenceOps
    extends SignalInferenceSyntax
    with EffectExp
    with ListOps
    with FunctionBlocksExp {
  this: SignalSyntax with SignalOps with VarOps =>

  override def new_inferred_signal[A:Manifest](
    f: Exp[SignalSynt[A]] => Exp[A]): Exp[SignalSynt[A]] = {

    val inferredDeps = inferDependencies(f)
    sig_ops_newSignal(inferredDeps, f)
  }

  def inferDependencies[A:Manifest](f: Exp[SignalSynt[A]] => Exp[A]): Exp[List[DepHolder]] = {
    val effects = effectSyms(lambdaToBlock1(f))
    val nestedSyms = aliasSyms(f).map(findDefinition(_)).map(effectSyms(_)).flatten
    val onlySyms = (effects ++ nestedSyms).filter { case Sym(x) => true; case _ => false }
    val defs = onlySyms.map(findDefinition(_)).collect {
      case Some(TP(_,Reflect(SigApply(x),_,_))) => x
      case Some(TP(_,Reflect(SigApplyDep(x,_),_,_))) => x
    }

    list_new(defs)
  }
}
