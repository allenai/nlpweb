package edu.knowitall
package nlpweb
package tool

import scala.Array.canBuildFrom
import common.Timing
import edu.knowitall.nlpweb.ToolIntent
import edu.knowitall.tool.chunk.OpenNlpChunker
import edu.knowitall.tool.chunk.Chunker
import edu.knowitall.tool.chunk.ChunkedToken
import unfiltered.request.HttpRequest
import com.googlecode.whatswrong.NLPInstance

object ChunkerIntent extends ToolIntent("chunker", List("opennlp")) {
  override val info = "Enter sentences to be chunked, one per line."
  lazy val opennlpChunker = new OpenNlpChunker()

  val chunkers = tools
  def getChunker(chunker: String): Chunker =
    chunker match {
      case "opennlp" => opennlpChunker
      case x => throw new IllegalArgumentException("unknown chunker: " + x)
    }

  def image(tokens: Seq[ChunkedToken]) = {
    import visualize.Whatswrong._
    val b64 = implicitly[CanWrite[Seq[ChunkedToken], Base64String]].write(tokens)
    "<img src=\"data:image/png;base64," + b64.string + "\">"
  }

  override def post[A](tool: String, text: String, params: Map[String, String]) = {
    val chunker = getChunker(tool)

    val lines = text.split("\n")
    val (chunkTime, chunkeds) = Timing.time(lines.map(chunker.chunk(_)))
    var colored = false
    ("time: " + Timing.Milliseconds.format(chunkTime),
    chunkeds.map {
      chunked => buildColoredTable(List("strings", "postags", "chunks"),
        chunked.map {
          case ChunkedToken(chunk, postag, string, offset) => if (chunk.startsWith("B")) colored = !colored; (Some(if (chunk.startsWith("O")) "pink" else if (colored) "lightgrey" else "white"), List(string, postag, chunk))
        })
    }.mkString("<br>\n") + (chunkeds map image).mkString("<p>", "\n", "</p>") )
  }
}
