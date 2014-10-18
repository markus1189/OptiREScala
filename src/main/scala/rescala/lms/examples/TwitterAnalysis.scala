package rescala.lms.examples

import java.io._
import java.awt.{ Color, Graphics2D }
import java.awt.Font
import rescala.events.ImperativeEvent
import rescala.IFunctions
import rescala.lms._
import rescala.Signal
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.event._
import scala.util.Random
import virtualization.lms.common.CompileScala

trait Resources {
  def stopwords: Set[String] = Source.fromFile("stopwords-en.txt").getLines.toSet
  def garbage: Set[String] =
    Set(
      "rt", ":)", ":-)", "^-^", "o:)", "o:-)", ":d", ":-d",
      "}:>", "}:->", ":x", "", ":-x", ";)", ";-)", ":(", ":-(",
      ":c", ":-c", ":'(", ":'-(", ":-/", ":>", ":->", ":p", ":-p",
      ":*", ":-*", ":o", ":-o", ":o", ":-o", ":v", ":-v", "8)",
      "8-)", "|:o", "o_o", "o_o", "<_<", "-_-", "<3", "(:", "):",
      "(-:", ")-:", "._.", "☺", ";d", "❤", "♡", "=(", "=)","♥",
      "&lt;3","&amp;"
    )
}

object Functions {
  val lowercase: Seq[String] => Seq[String] = _.map(_.toLowerCase)

  val toWords: Seq[String] => Seq[Seq[String]] = _.map(_.split("\\s+").toSeq)

  def countWords[A]: Seq[Seq[A]] => Seq[Int] = seq => seq.map(_.length)

  val sumIt: Seq[Int] => Int = _.sum

  val randomWeights: Seq[String] => Seq[(String,Int)] =
    _.map { w => (w,scala.util.Random.nextInt(10))}

  val wordFrequency: Seq[String] => Map[String,Int] =
    _.foldLeft(Map[String,Int]().withDefaultValue(0)) { (acc,w) =>
      acc + (w -> (acc(w) + 1))
    }

  val normalizeFrequency: Map[String,Int] => Map[String,Float] = freqMap => {
    val maxFreq = if (freqMap.isEmpty) 1 else freqMap.values.max
    freqMap.mapValues { x => x.toFloat / maxFreq }
  }

  val createLabels: Seq[(String,Float)] => Seq[Label] = wordsWithWeight =>
  wordsWithWeight.map { case (word,weight) =>
    val label = new Label(" " + word + " ")
    label.font = new Font(label.font.getName, Font.PLAIN, (weight*100).toInt)
    label
  }

  val removeWords: Set[String] => Seq[String] => Seq[String] =
    ws => _.filterNot(ws)
}

trait TwitterAnalysis extends SimpleSwingApplication {
  this: HasProgram =>

  val TWEET_DELAY = 1
  val WINDOW_SIZE = 1000

  val tweetEvent = new ImperativeEvent[String]
  val tweetsInput: Signal[Seq[String]] = tweetEvent.last(WINDOW_SIZE)

  val tweets: Seq[String] = Source.fromFile("tweets.csv").getLines.toSeq
  tweets.take(WINDOW_SIZE).foreach(tweetEvent(_)) // Fill the first window

  def top = new MainFrame { // top is a required method
    title = "Twitter Word Cloud"

    val panel = new FlowPanel

    program(tweetsInput).changed += { labels =>
      panel.contents.clear
      panel.contents ++= labels
      panel.revalidate()
      panel.repaint()
    }

    contents = panel
    size = new Dimension(1200, 700)
  }

  val fut = future {
    tweets.drop(WINDOW_SIZE).foreach { tweet =>
      Thread.sleep(TWEET_DELAY)
      tweetEvent(tweet)
    }
  }
}

trait HasProgram { def program: Signal[Seq[String]] => Signal[Seq[Label]] }

object NormalTwitterAnalysis extends TwitterAnalysis with HasProgram with Resources {
  def program = input => {
    input.
      map(Functions.toWords).
      map((_.flatten)).
      map(Functions.lowercase).
      // stemming).
      map(Functions.removeWords(stopwords)).
      map(Functions.removeWords(garbage)).
      map(Functions.wordFrequency).
      map(_.filter { case (word,weight) => weight > 1 }).
      map(Functions.normalizeFrequency).
      map(_.toSeq).
      map(Functions.createLabels)
  }
}

object LMSTwitterAnalysis extends TwitterAnalysis with HasProgram with Resources {
  def program = compiledProg

  val prog = new LMSTwitterProgram with ReactiveDSLExp with CompileScala { self =>
    override val codegen = new ReactiveDSLGen {
      val IR: self.type = self
    }
  }

  val out = new StringWriter
  prog.codegen.emitSource(prog.f, "F", new PrintWriter(out))
  println(out.toString)

  val compiledProg = prog.compile(prog.f).apply( () )

  trait LMSTwitterProgram extends ReactiveDSL {
    def f(x: Rep[Unit]): Rep[Signal[Seq[String]] => Signal[Seq[Label]]] =
      (input: Rep[Signal[Seq[String]]]) => {
      input.
        fuseMapRep(unit(Functions.toWords)).
        fuseMapRep(unit {
          (x: Seq[Seq[String]]) => x.flatten
        }).
        fuseMapRep(unit(Functions.lowercase)).
        // stemming).
        fuseMapRep(unit(Functions.removeWords(stopwords))).
        fuseMapRep(unit(Functions.removeWords(garbage))).
        fuseMapRep(unit(Functions.wordFrequency)).
        fuseMapRep(unit {
          (x: Map[String,Int]) => x.filter { case (word,weight) => weight > 1 }}
        ).
        fuseMapRep(unit(Functions.normalizeFrequency)).
        fuseMapRep(unit((x: Map[String,Float]) => x.toSeq)).
        fuseMapRep(unit(Functions.createLabels))
    }
  }
}
