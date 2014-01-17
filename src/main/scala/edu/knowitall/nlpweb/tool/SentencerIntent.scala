package edu.knowitall
package nlpweb
package tool

import edu.knowitall.nlpweb.ToolIntent
import edu.knowitall.tool.segment.RemoteSegmenter
import edu.knowitall.tool.segment.Segmenter

import common.Timing

import scala.concurrent.ExecutionContext.Implicits.global

object SentencerIntent
extends ToolIntent[Segmenter]("sentence", List("opennlp" -> "OpenNlpSentencer")) {
  override val info = "Enter a single block of text (paragraph) to split into sentences."

  def constructors= PartialFunction.empty[String, Segmenter] /* = {
    case "OpenNlpSentencer" => new OpenNlpSentencer()
  }*/
  override def remote(url: java.net.URL) = new RemoteSegmenter(url.toString)

  override def post[A](shortToolName: String, text: String, params: Map[String, String]) = {
    val sentencer = getTool(nameMap(shortToolName))

    val (sentencerTime, sentenced) = Timing.time(sentencer.segmentTexts(text))
    ("time: " + Timing.Milliseconds.format(sentencerTime),
    sentenced.map("<li>" + _).mkString("<ol>\n", "\n", "</ol>"))
  }
}
