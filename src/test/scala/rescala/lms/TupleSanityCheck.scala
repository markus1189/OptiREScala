package rescala.lms

import org.scalatest._
import virtualization.lms.common.CompileScala
import scala.util.matching._
import scala.virtualization.lms.common.ScalaGenTupleOps

class TupleSanityCheck extends WordSpec with Matchers {
  "Tuples" can {
    "be created" in {

      trait TupleProg extends ReactiveDSL {
        def f(x : Rep[Unit]): Rep[(Int,String)] = (42,"Hello")
      }

      val prog = new TupleProg with ReactiveDSLExp with CompileScala { self =>
        override val codegen = new ReactiveDSLGen with ScalaGenTupleOps {
          val IR: self.type = self
        }
      }

      val out = new java.io.StringWriter();
      prog.codegen.emitSource(prog.f, "F", new java.io.PrintWriter(out))
      println(out.toString)

      val s = prog.compile(prog.f).apply( () )

      s should equal((42,"Hello"))
    }

  }
}
