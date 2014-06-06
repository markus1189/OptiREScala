package rescala.lms.dsl

import org.scalatest._
import virtualization.lms.common.CompileScala
import rescala.lms._
import rescala.{Var => REVar,Signal => RESignal, SignalSynt => RESignalSynt}

import scala.util.matching._

class MapFusionSpec extends WordSpec with Matchers {
  "MapFusion" should {
    "eliminate intermediate map calls" in {
      val prog = new MapFusionProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen {
          val IR: self.type = self
        }
      }

      val s = prog.compile(prog.f).apply( () )

      // Check the result value
      s.get should equal(1+1+2+3)

      // Check result of code generation: only one call to map
      val out = new java.io.StringWriter();
      prog.codegen.emitSource(prog.f, "F", new java.io.PrintWriter(out))
      out.toString.lines.toList.filter(_.contains(".map")) should have length(1)
    }
  }
}

trait MapFusionProg extends ReactiveDSL {
  def f(x : Rep[Unit]) = {
    val v: Rep[RESignal[Int]] = Signal(List()) { s: Rep[RESignalSynt[Int]] => 1}
    v.fuseMap(_+1).fuseMap(_+2).fuseMap(_+3)
  }
}
