package rescala.lms

import scala.virtualization.lms.common._
import rescala.lms.syntaxops._
import rescala.lms.optimizations._
import rescala.lms.auxiliary._

trait ReactiveDSL extends Reactivity with ScalaOpsPkg with LiftScala

trait ReactiveDSLExp extends ReactiveDSL with ReactivityExp with ScalaOpsPkgExp

trait ReactiveDSLGen extends ScalaGenReactivity with ScalaCodeGenPkg {
  val IR: ReactiveDSLExp with ScalaOpsPkgExp
}

trait Reactivity
    extends VarSyntax
    with SignalSyntax
    with SignalInferenceSyntax
    with MapFusionSyntax
    with FibonacciSyntax
    with LMSSyntax  // Include some syntax from LMS

trait LMSSyntax
    extends Functions
    with MapOps

trait ReactivityExp
    extends Reactivity
    with VarOps
    with SignalOps
    with SignalInferenceOps
    with MapFusionOps
    with FibonacciOps
    with LMSProvided  // Include many traits provided my LMS

trait LMSProvided
    extends ListOpsExp
    with SeqOpsExp
    with MapOpsExp
    with EffectExp
    with OrderingOpsExp
    with NumericOpsExp
    with IfThenElseExp
    with PrimitiveOpsExp

trait ScalaGenReactivity extends ScalaGenReactiveBase
    with ScalaGenVars
    with ScalaGenSignals
    with ScalaGenMapFusion
    with ScalaGenFibonacci
    with ScalaGenMapOps
    with LMSProvidedGen {
  val IR: ReactivityExp
  import IR._
}

trait LMSProvidedGen
    extends ScalaGenEffect
    with ScalaGenIfThenElse
