package edu.knowitall
package nlpweb
package tool

import edu.knowitall.nlpweb.DotIntent
import edu.knowitall.nlpweb.ToolIntent
import edu.knowitall.nlpweb.visualize.Whatswrong.CanWrite
import edu.knowitall.repr.sentence.Dependencies
import edu.knowitall.repr.sentence.Postags
import edu.knowitall.repr.sentence.Sentence
import edu.knowitall.tool.parse.DependencyParser
import edu.knowitall.tool.parse.graph.DependencyGraph
import edu.knowitall.tool.parse.graph.DependencyNode
import edu.knowitall.tool.parse.graph.DependencyPattern
import edu.knowitall.tool.parse.RemoteDependencyParser
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.stem.Lemmatized
import edu.knowitall.tool.stem.MorphaStemmer

import common.Timing
import visualize.Whatswrong.Base64String
import visualize.Whatswrong.CanWrite
import visualize.Whatswrong.writeGraphic2Base64

import scala.annotation.migration
import scala.collection.JavaConversions.asJavaCollection
import scala.concurrent.ExecutionContext.Implicits.global

object ParserIntent
extends ToolIntent[DependencyParser]("parse",
    List(
      "stanford" -> "StanfordParser",
      "malt" -> "MaltParser",
      "clear" -> "ClearParser",
      "deserialize" -> "DeserializeParser")) {
  implicit def stemmer = MorphaStemmer
  override val info = "Enter a single sentence to be parsed."

  def constructors: PartialFunction[String, DependencyParser] = {
    // case "StanfordParser" => new StanfordParser()
    // case "MaltParser" => new MaltParser()
    // case "ClearParser" => new ClearParser()
    case "DeserializeParser" => new DependencyParser {
      override def postagger = throw new UnsupportedOperationException
      override def dependencyGraphPostagged(tokens: Seq[edu.knowitall.tool.postag.PostaggedToken]) = throw new UnsupportedOperationException
      override def dependencyGraph(pickled: String) = DependencyParser.multilineStringFormat.read(pickled)
    }
  }
  override def remote(url: java.net.URL) = new RemoteDependencyParser(url.toString)

  override def config[A](req: unfiltered.request.HttpRequest[A], tool: String) = {
    val pattern =
      if (req.parameterNames contains "pattern") req.parameterValues("pattern").headOption
      else None
    config(
      pattern,
      req.parameterNames.contains("collapsed"),
      req.parameterNames.contains("collapseNounGroups"),
      req.parameterNames.contains("collapsePrepOf"),
      req.parameterNames.contains("collapseWeakLeaves"))
  }

  def config(pattern: Option[String], collapsed: Boolean, collapseNounGroups: Boolean, collapsePrepOf: Boolean, collapseWeakLeaves: Boolean): String = """
    pattern: <input name="pattern" type="input" size="60" value="""" + pattern.getOrElse("") + """" /><br />
    <input name="collapsed" type="checkbox" value="true" """ + (if (collapsed) """checked="true" """ else "") + """ /> Collapsed<br />
    <input name="collapseNounGroups" type="checkbox" value="true" """ + (if (collapseNounGroups) """checked="true" """ else "") + """/> Collapse Noun Groups<br />
    <input name="collapsePrepOf" type="checkbox" value="true" """ + (if (collapsePrepOf) """checked="true" """ else "") + """/> Collapse Prep Of<br />
    <input name="collapseWeakLeaves" type="checkbox" value="true" """ + (if (collapseWeakLeaves) """checked="true" """ else "") + """/> Collapse Weak Leaves<br />
    <br />"""

  def whatswrongImage(graph: DependencyGraph) = {
    try {
      import visualize.Whatswrong._
      val b64 = implicitly[CanWrite[DependencyGraph, Base64String]].write(graph)
      "<img src=\"data:image/png;base64," + b64.string + "\">"
    }
    catch {
      case e: Throwable => System.err.println("Could not build image for: " + DependencyGraph.singlelineStringFormat.write(graph)); ""
    }
  }

  override def post[A](shortToolName: String, text: String, params: Map[String, String]) = {
    val parser = getTool(nameMap(shortToolName))
    val pattern = ""
    var (parseTime, (postags: Seq[PostaggedToken], graph)) = parser.synchronized {
      Timing.time(
        parser match {
          case parser: DependencyParser =>
            val (postags, graph) = parser.dependencyGraph(text)
            if (/*params.getOrElse("collapsed", "")*/"true" == "true")
              (postags, graph.collapse)
            else
              (postags, graph)
        })
    }

    val lemmatizedPostags: Seq[Lemmatized[PostaggedToken]] = postags map MorphaStemmer.lemmatizePostaggedToken
    val tokenizedGraph = graph.tokenized(postags map MorphaStemmer.lemmatizePostaggedToken)

    val (matches, (nodes, edges)) = if ((params.keys contains "pattern") && !params("pattern").isEmpty) {
      val pattern = new DependencyPattern.StringFormat().read(params("pattern").trim)
      val matches = pattern(tokenizedGraph)
      val nodes = for (m <- matches; v <- m.bipath.nodes) yield v
      val edges = for (m <- matches; e <- m.bipath.edges) yield e
      (matches, (nodes, edges))
    }
    else (List(), (List(), List()))

    val hack: Seq[PostaggedToken] = postags // scala compiler error?
    val sentence = new Sentence(text) with Dependencies with Postags {
      override def tokens = hack
      override def dgraph = graph
    }
    val dot = Dependencies.DotWriter.dotWithHighlights(sentence, if (text.length > 100) text.substring(0, 100) + "..." else text, Set.empty, Set.empty)
    val base64Image = DotIntent.dotbase64(dot, "png")

    ("parse time: " + Timing.Milliseconds.format(parseTime),
      whatswrongImage(graph) + "<br/><br/>" +
      "<img src=\"data:image/png;base64," + base64Image + "\" /><br>" +
      "<pre>" + matches.map(m => "match with node groups (" + m.nodeGroups.mkString(" ") +
      ") and edge groups (" + m.edgeGroups.mkString(" ") + ")<br>").mkString("") + "<br>" +
      text + "\n\n" + DependencyGraph.singlelineStringFormat.write(graph) + "\n\n" +
      graph.toString + "\n\n" +
      dot + "</pre>")
  }
}
