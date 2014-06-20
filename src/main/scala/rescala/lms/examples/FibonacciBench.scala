package rescala.lms.examples

import rescala.lms._
import rescala.{Signal => RESignal, SignalSynt => RESignalSynt, Var => REVar}
import virtualization.lms.common.CompileScala

object FibonacciLMS extends App {
 val prog = new FibonacciLMSProg with ReactiveDSLExp with CompileScala { self =>
    override val codegen = new ReactiveDSLGen { val IR: self.type = self }
  }

  val s = prog.compile(prog.f).apply( () )
}

object FibonacciREScala extends App {
  import rescala.lms.auxiliary.FibonacciImpl.fib

  val fibBaseVal = 42l
  val root = REVar(1l)
  val s1: RESignalSynt[Long] = rescala.SignalSynt(root){ s: RESignalSynt[Long] => root(s) + fib(fibBaseVal)}


  val s2 = s1.map { x => x + fib(fibBaseVal) }
  val s3 = s2.map { x => x + fib(fibBaseVal) }
  val s4 = s3.map { x => x + fib(fibBaseVal) }
  val s5 = s4.map { x => x + fib(fibBaseVal) }

  println(s5.get)

  root.set(2l)

  println(s5.get)
}

trait FibonacciLMSProg extends ReactiveDSL {
  val fibBaseVal = unit(42l)

  def f(x : Rep[Unit]) = {
    val root = Var(unit(1l))

    val s1: Rep[RESignal[Long]] = Signal(root) { s: Rep[RESignalSynt[Long]] =>
      root(s) + fib(fibBaseVal)
    }

    val s2: Rep[RESignal[Long]] = s1.fuseMap { x => x + fib(fibBaseVal) }
    val s3: Rep[RESignal[Long]] = s2.fuseMap { x => x + fib(fibBaseVal) }
    val s4: Rep[RESignal[Long]] = s3.fuseMap { x => x + fib(fibBaseVal) }
    val s5: Rep[RESignal[Long]] = s4.fuseMap { x => x + fib(fibBaseVal) }

    println(s5.get)

    root.set(unit(2l))

    println(s5.get)
  }
}
