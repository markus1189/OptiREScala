package react.lms

import scala.virtualization.lms.common._
import react.lms.syntaxops._
import react.lms.optimizations._

trait ReactiveDSL extends Reactivity with ScalaOpsPkg with LiftScala

trait ReactiveDSLExp extends ReactiveDSL with ReactivityExp with ScalaOpsPkgExp

trait ReactiveDSLGen extends ScalaGenReactivity with ScalaCodeGenPkg {
  val IR: ReactiveDSLExp with ScalaOpsPkgExp
}

trait Reactivity
    extends VarSyntax
    with SignalSyntax
    with SignalInferenceSyntax
    with TupleOps

trait ReactivityExp
    extends Reactivity
    with VarOps
    with SignalOps
    with SignalInferenceOps
    with ListOpsExp
    with SeqOpsExp
    with EffectExp
    with TupleOpsExp

trait ScalaGenReactivity extends ScalaGenReactiveBase
    with ScalaGenEffect
    with ScalaGenVars
    with ScalaGenSignals
    with ScalaGenTupleOps {
  val IR: ReactivityExp
  import IR._
}
