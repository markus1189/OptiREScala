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

object TwitterAnalysis extends App {
  val BATCH_SIZE = 100
  val SLEEP_DURATION = 1000

  val tweetEvent = new ImperativeEvent[String]

  val tweets = Source.fromFile("tweets.csv").getLines.toSeq

  val tweetsInput: Signal[Seq[String]] = tweetEvent.last(100)

  val processedTweets =
    tweetsInput.
      map(Functions.lowercase).
      map(Functions.toWords).
      map(Functions.countWords).
      map(Functions.sumIt)

  processedTweets.changed += { x => println(x) }

  val fut = future {
    val (batch,remaining) = tweets.splitAt(BATCH_SIZE)
    batch.foreach(tweetEvent(_))

    remaining.foreach { tweet =>
      Thread.sleep(SLEEP_DURATION)
      tweetEvent(tweet)
    }
  }

  Await.ready(fut, Duration.Inf)
}

object Functions {
  val lowercase: Seq[String] => Seq[String] = seq => seq.map(_.map(_.toLower))
  val toWords: Seq[String] => Seq[Seq[String]] = _.map(_.split("\\s+").toSeq)
  def countWords[A]: Seq[Seq[A]] => Seq[Int] = seq => seq.map(_.length)
  val sumIt: Seq[Int] => Int = _.sum
}

object SimpleGUI extends SimpleSwingApplication {
  def top = new MainFrame { // top is a required method
    title = "Twitter Word Cloud"

    val words = Seq("berzelius","intrepidly","wren","salivary","tar","straplesses","williamson","servitude","snowdrift","tigers","sumach","exile","molnar","sandbag","semester","japanese","faithful","blunders","freights","falsetto","dewy","pernod","scrambler","conventionality","forenames","hackneying","bust","attar","johnathan","spontaneous","elaboration","cubicles","tianjin","spicy","funerals","cosmologist","smacker","isuzu","klondikes","registrar","landmass","caribou","micky","walkout","connolly","shrieks","medicaids","archway","jersey","brain","beetle","viva","furriest","sinewy","witting","butterfingers","gentry","seeding","underwear","barrelled","brooke","redcoats","extravert","mutual","inferior","timidest","incidentally","virtue","designation","acquaint","yuck","palaces","regal","grommets","copernicus","excommunications","inalienable","consequence","elongates","barrenness","chorus","grommet","agile","chichi","episcopalians","bathmat","mousiness","rheumier","wilson","guppy","selector","details","thornton","lean","parker","puncture","anorexia","hals","proscribe")

    def randomWeights(words: Seq[String]): Seq[(String,Int)] = words.map { w => (w,scala.util.Random.nextInt(50))}

    def createLabels(wordsWithWeight: Seq[(String,Int)]): Seq[Label] =
      wordsWithWeight.map { case (word,weight) =>
        val label = new Label(word)
        label.font = new Font(label.font.getName, Font.PLAIN, weight)
        label
      }

    val fromWords = (createLabels _).compose(randomWeights _)

    val panel = new FlowPanel { contents ++= fromWords(words)}

    contents = panel
    size = new Dimension(300, 200)

    future {
      while (true) {
        Thread.sleep(5000)
        panel.contents.clear()
        panel.contents ++= fromWords(words)
        Thread.sleep(1000)
        panel.revalidate()
      }
    }
  }
}
