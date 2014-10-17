package rescala.lms.examples

import java.awt.Font
import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{ Color, Graphics2D }
import scala.util.Random

import rescala.events.ImperativeEvent
import rescala.IFunctions
import rescala.Signal
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source

object TwitterAnalysis extends SimpleSwingApplication {
  val BATCH_SIZE = 1000
  val SLEEP_DURATION = 125

  val tweetEvent = new ImperativeEvent[String]

  val tweets = Source.fromFile("tweets.csv").getLines.toSeq

  val tweetsInput: Signal[Seq[String]] = tweetEvent.last(100)

  val processedTweets =
    tweetsInput.
      map(Functions.toWords).
      map(_.flatten).
      map(Functions.lowercase).
      // symbols like <3, :), etc
      // stopwords
      // stemming
      map(Functions.wordFrequency).
      map(_.filter { case (word,weight) => weight > 1 }).
      map(Functions.normalizeFrequency).
      map(_.toSeq).
      map(Functions.createLabels)

  def top = new MainFrame { // top is a required method
    title = "Twitter Word Cloud"

    val panel = new FlowPanel

    processedTweets.changed += { labels =>
      panel.contents.clear
      panel.contents ++= labels
      panel.revalidate()
      panel.repaint()
    }

    contents = panel
    size = new Dimension(300, 200)
  }

  val fut = future {
    tweets.foreach { tweet =>
      Thread.sleep(SLEEP_DURATION)
      tweetEvent(tweet)
    }
  }
}

object Functions {
  val lowercase: Seq[String] => Seq[String] = _.map(_.map(_.toLower))

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
      val label = new Label(word)
      label.font = new Font(label.font.getName, Font.PLAIN, (weight*100).toInt)
      label
    }
}
