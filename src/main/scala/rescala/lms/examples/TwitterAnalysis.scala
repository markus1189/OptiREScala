package rescala.lms.examples

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
    title = "A Sample Scala Swing GUI"

    // declare Components here
    val label = new Label {
      text = "I'm a big label!."
      font = new Font("Ariel", java.awt.Font.ITALIC, 24)
    }
    val button = new Button {
      text = "Throw!"
      foreground = Color.blue
      background = Color.red
      borderPainted = true
      enabled = true
      tooltip = "Click to throw a dart"
    }
    val toggle = new ToggleButton { text = "Toggle" }
    val checkBox = new CheckBox { text = "Check me" }
    val textField = new TextField {
      columns = 10
      text = "Click on the target!"
    }
    val textArea = new TextArea {
      text = "initial text\nline two"
      background = Color.green
    }
    val gridPanel = new GridPanel(1, 2) {
      contents += checkBox
      contents += label
      contents += textArea
    }

    // choose a top-level Panel and put components in it
    // Components may include other Panels
    contents = new BorderPanel {
      layout(gridPanel) = North
      layout(button) = West
      layout(toggle) = East
      layout(textField) = South
    }
    size = new Dimension(300, 200)
    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }

    // specify which Components produce events of interest
    listenTo(button)
    listenTo(toggle)

    // react to events
    reactions += {
      case ButtonClicked(component) if component == button =>
        val x = Random.nextInt(100)
        val y = Random.nextInt(100)
        val c = new Color(Random.nextInt(Int.MaxValue))
        textField.text = s"Dart thrown at $x, $y"
      case ButtonClicked(component) if component == toggle =>
        toggle.text = if (toggle.selected) "On" else "Off"
      case MouseClicked(_, point, _, _, _) =>
        textField.text = (s"You clicked in the Canvas at x=${point.x}, y=${point.y}.")
    }
  }
}
