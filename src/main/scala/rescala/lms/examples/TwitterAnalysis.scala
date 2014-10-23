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
import scala.collection.mutable.{Set => MutableSet}

trait Resources {
  def stopwords: MutableSet[String] = MutableSet(Source.fromFile("stopwords-en.txt").getLines.toSeq: _*)
  def garbage: MutableSet[String] =
    MutableSet(
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
    val maxFreq = if (freqMap.isEmpty) 1 else freqMap.values.head
    freqMap.mapValues { x => x.toFloat / maxFreq }
  }

  val createLabels: Seq[(String,Float)] => Seq[Label] = wordsWithWeight =>
  wordsWithWeight.map { case (word,weight) =>
    val label = new Label(" " + word + " ")
    label.font = new Font(label.font.getName, Font.PLAIN, (weight*100).toInt)
    label
  }

  val removeWords: MutableSet[String] => Seq[String] => Seq[String] =
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

    program(tweetsInput).map(Functions.createLabels).changed += { labels =>
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

trait HasProgram { def program: Signal[Seq[String]] => Signal[Seq[(String,Float)]] }

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
      map(_.toSeq)
  }
}

object LMSTwitterAnalysis extends TwitterAnalysis with HasProgram with Resources {
  def program = x => compiledProg(stopwords,garbage,x)

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
    def f(x: Rep[Unit]): Rep[((MutableSet[String],MutableSet[String],Signal[Seq[String]])) => Signal[Seq[(String,Float)]]] =
      fun { (sw: Rep[MutableSet[String]],gb: Rep[MutableSet[String]],input: Rep[Signal[Seq[String]]]) => {
        input.
          fuseMap { (x: Rep[Seq[String]]) => x.map(_.split("\\s+").toSeq) }.
          fuseMap { (x: Rep[Seq[Seq[String]]]) => x.flatten }.
          fuseMap { (x: Rep[Seq[String]]) => x.map(_.toLowerCase) }.
          // stemming).
          fuseMap { (x: Rep[Seq[String]]) => x.filterNot(y => sw.contains(y))}.
          fuseMap { (x: Rep[Seq[String]]) => x.filterNot(y => gb.contains(y))}.
          fuseMap { (x: Rep[Seq[String]]) =>
            x.foldLeft(Map[String,Int]().withDefaultValue(0)) { accAndw =>
              val acc = accAndw._1
              val w = accAndw._2
              acc.insert((w, acc(w)+1))
            }
          }.
          fuseMap { (x: Rep[Map[String,Int]]) =>
            x.filter { wordAndWeight => wordAndWeight._2 > 1 }
          }.
          fuseMapRep { (x: Rep[Map[String,Int]]) =>
            val maxFreq = if (x.isEmpty) 1 else x.values.max
            x.mapValues { x => x.toFloat / maxFreq }
          }.
          fuseMap((x: Rep[Map[String,Float]]) => x.toSeq)
      }
      }
  }
}
