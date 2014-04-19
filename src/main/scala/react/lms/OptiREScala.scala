package react.lms

import scala.virtualization.lms.common._
import react.lms.syntaxops._

trait ReactiveDSL extends Reactivity with ScalaOpsPkg with LiftScala

trait ReactiveDSLExp extends ReactiveDSL with ReactivityExp with ScalaOpsPkgExp

trait ReactiveDSLGen extends ScalaGenReactivity with ScalaCodeGenPkg {
  val IR: ReactiveDSLExp with ScalaOpsPkgExp
}

trait Reactivity extends VarSyntax

trait ReactivityExp
    extends Reactivity
    with VarOps
    with ListOpsExp
    with SeqOpsExp
    with EffectExp

trait ScalaGenReactivity extends ScalaGenReactiveBase
    with ScalaGenEffect
    with ScalaGenVars {
  val IR: ReactivityExp
  import IR._
}
