package react.lms

import scala.virtualization.lms.common._
import react.lms.syntaxops._

trait ReactiveDSL extends Reactivity with ScalaOpsPkg with LiftScala

trait ReactiveDSLExp extends ReactiveDSL with ReactivityExp with ScalaOpsPkgExp

trait ReactiveDSLGen extends ScalaGenReactivity with ScalaCodeGenPkg {
  val IR: ReactiveDSLExp with ScalaOpsPkgExp
}

trait Reactivity
    extends VarSyntax
    with SignalSyntax

trait ReactivityExp
    extends Reactivity
    with VarOps
    with SignalOps
    with ListOpsExp
    with SeqOpsExp
    with EffectExp

trait ScalaGenReactivity extends ScalaGenReactiveBase
    with ScalaGenEffect
    with ScalaGenVars
    with ScalaGenSignals {
  val IR: ReactivityExp
  import IR._
}
