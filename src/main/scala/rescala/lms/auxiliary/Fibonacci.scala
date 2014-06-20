package rescala.lms.auxiliary

import scala.virtualization.lms.common.{Base,BaseExp,IfThenElseExp,OrderingOpsExp,NumericOpsExp,PrimitiveOpsExp}

import rescala.lms.syntaxops.ScalaGenReactiveBase

object FibonacciImpl {
  def fib(x: Long): Long = x match {
    case x if x < 2 => 1
    case _ => fib(x-1) + fib(x-2)
  }
}

trait FibonacciSyntax extends Base {
  def fib(x: Rep[Long]): Rep[Long]
}

trait FibonacciOps extends BaseExp {

  this: FibonacciSyntax
      with IfThenElseExp
      with OrderingOpsExp
      with NumericOpsExp
      with PrimitiveOpsExp =>

  case class FibonacciApp(x: Exp[Long]) extends Def[Long]

  override def fib(x: Exp[Long]): Exp[Long] = FibonacciApp(x)
}

trait ScalaGenFibonacci extends ScalaGenReactiveBase {
  val IR: FibonacciOps
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case FibonacciApp(x) =>
      emitValDef(sym, "rescala.lms.auxiliary.FibonacciImpl.fib(" + quote(x) + ")")
    case _ => super.emitNode(sym,node)
  }
}
