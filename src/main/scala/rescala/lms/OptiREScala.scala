package rescala.lms

import scala.virtualization.lms.common._
import rescala.lms.syntaxops._
import rescala.lms.optimizations._

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

trait ReactivityExp
    extends Reactivity
    with VarOps
    with SignalOps
    with SignalInferenceOps
    with MapFusionOps
    with ListOpsExp
    with SeqOpsExp
    with EffectExp

trait ScalaGenReactivity extends ScalaGenReactiveBase
    with ScalaGenEffect
    with ScalaGenVars
    with ScalaGenSignals
    with ScalaGenMapFusion {
  val IR: ReactivityExp
  import IR._
}
