package rescala.lms.optimizations

import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenEffect, FunctionsExp, ForwardTransformer, CompileScala, BaseExp}
import scala.virtualization.lms.internal.Expressions
import language.implicitConversions
import rescala.lms.syntaxops._
import rescala.lms._

import rescala.{Signal => RESignal, SignalSynt => RESignalSynt, DepHolder}

object FusionTransformers {

  // sig.apply(sig2) --> sig
  def sigApplyDepTransformer(prog: CompileScala with ReactiveDSLExp) = {
    new ForwardTransformer {
      val IR: prog.type = prog
      import IR._

      var inMappedSignal: Boolean = false

      override def transformStm(stm: Stm) = {
        stm match {
          case TP(_,Reflect(SigApplyDep(s1,_),_,_)) => s1 // x1(x2) --> x1
          case _ => super.transformStm(stm)
        }}
    }
  }

  // Signal(d) { s => d(s) + 1 } --> d.map { x => x() + 1 }
  def sigExprToMap(prog: CompileScala with ReactiveDSLExp) = {
    new ForwardTransformer {
      val IR: prog.type = prog
      import IR._
      override def transformStm(stm: Stm) = {
        stm match {
          case TP(_,ssc@SingleDepSignalCreation(d@Sym(_),Def(Lambda(_,_,exprBdy)),_)) =>
            val freshLambdaParam = fresh(ssc.tB)
            subst += d -> freshLambdaParam // replace d with λ arg inside expression
            val transformed: Block[Any] = sigApplyDepTransformer(prog).transformBlock(transformBlock(exprBdy))
            val constant: Exp[Any] => Exp[Any] = s => transformed.res
            sig_ops_map_new(d, Lambda(constant, freshLambdaParam, transformed))
          case _ => super.transformStm(stm)
        }}
    }
  }
}
