/*
 * Copyright 2009 Robey Pointer <robeypointer@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.lag.configgy

import scala.collection.mutable.Stack
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
import net.lag.extensions._


/**
 * An exception thrown when parsing a config file, if there was an error
 * during parsing. The `reason` string will contain the parsing
 * error details.
 */
class ParseException(reason: String, cause: Throwable) extends Exception(reason, cause) {
  def this(reason: String) = this(reason, null)
  def this(cause: Throwable) = this(null, cause)
}


private[configgy] class ConfigParser(var attr: Attributes, val importer: Importer) extends RegexParsers {

  val sections = new Stack[String]
  var prefix = ""

  // Stack reversed iteration order from 2.7 to 2.8!!
  def sectionsString = sections.toList.reverse.mkString(".")

  // whiteSpace and stringToken can span very long input. java.util.regex recurses once per
  // iteration of a greedy group loop such as (a|b)* to keep a backtracking point, overflowing
  // the stack after a few thousand characters. Instead each is an "unrolled loop":
  //
  //   filler*+ (separator filler*+)*+
  //
  // filler is a single character class, which a quantifier matches without recursion.
  // separator is a comment or escape sequence. Every quantifier is possessive (*+), which keeps
  // no backtracking points, so the group loop doesn't recurse either. That is safe because
  // filler and separator never match at the same position, so backtracking couldn't find
  // another match.

  override val whiteSpace = {
    val blanks = """\s*+"""
    // Must end in a newline, so a trailing comment without one is a parse error.
    val comment = """#[^\n]*+\n"""
    // May match empty, which skips nothing, same as no match.
    (blanks + "(?:" + comment + blanks + ")*+").r
  }

  val numberToken: Parser[String] = """-?\d+(\.\d+)?""".r

  val stringToken: Parser[String] = {
    val plainChars = """[^\\"]*+"""
    // Backslash followed by u and 4 hex digits, x and 2 hex digits, or any other character
    // (including a newline).
    val escape = """\\(?:u[0-9a-fA-F]{4}|x[0-9a-fA-F]{2}|[^ux])"""
    ("\"" + plainChars + "(?:" + escape + plainChars + ")*+" + "\"").r
  }

  val identToken: Parser[String] = """([\da-zA-Z_][-\w]*)(\.[a-zA-Z_][-\w]*)*""".r
  val assignToken: Parser[String] = """=|\?=""".r
  val tagNameToken: Parser[String] = """[a-zA-Z][-\w]*""".r


  def root = rep(includeFile | includeOptFile | assignment | toggle | sectionOpen | sectionClose |
                 sectionOpenBrace | sectionCloseBrace)

  def includeFile = "include" ~> string ^^ {
    case filename: String =>
      new ConfigParser(attr.makeAttributes(sectionsString), importer) parse importer.importFile(filename)
  }

  def includeOptFile = "include?" ~> string ^^ {
    case filename: String =>
      new ConfigParser(attr.makeAttributes(sections.mkString(".")), importer) parse importer.importFile(filename, false)
  }

  def assignment = identToken ~ assignToken ~ value ^^ {
    case k ~ a ~ v => if (a match {
      case "=" => true
      case "?=" => ! attr.contains(prefix + k)
    }) v match {
      case x: Long => attr(prefix + k) = x
      case x: String => attr(prefix + k) = x
      case x: Array[String] => attr(prefix + k) = x
      case x: Boolean => attr(prefix + k) = x
    }
  }

  def toggle = identToken ~ trueFalse ^^ { case k ~ v => attr(prefix + k) = v }

  def sectionOpen = "<" ~> tagNameToken ~ rep(tagAttribute) <~ ">" ^^ {
    case name ~ attrList => openBlock(name, attrList)
  }
  def tagAttribute = opt(whiteSpace) ~> (tagNameToken <~ "=") ~ string ^^ { case k ~ v => (k, v) }
  def sectionClose = "</" ~> tagNameToken <~ ">" ^^ { name => closeBlock(Some(name)) }

  def sectionOpenBrace = tagNameToken ~ opt("(" ~> rep(tagAttribute) <~ ")") <~ "{" ^^ {
    case name ~ attrListOption => openBlock(name, attrListOption.getOrElse(Nil))
  }
  def sectionCloseBrace = "}" ^^ { x => closeBlock(None) }

  private def openBlock(name: String, attrList: List[(String, String)]) = {
    val parent = if (sections.size > 0) attr.makeAttributes(sectionsString) else attr
    sections push name
    prefix = sectionsString + "."
    val newBlock = attr.makeAttributes(sectionsString)
    for ((k, v) <- attrList) k match {
      case "inherit" =>
        newBlock.inheritFrom = Some(if (parent.getConfigMap(v).isDefined) parent.makeAttributes(v) else attr.makeAttributes(v))
      case _ =>
        throw new ParseException("Unknown block modifier")
    }
  }

  private def closeBlock(name: Option[String]) = {
    if (sections.isEmpty) {
      failure("dangling close tag")
    } else {
      val last = sections.pop
      if (name.isDefined && last != name.get) {
        failure("got closing tag for " + name.get + ", expected " + last)
      } else {
        prefix = if (sections.isEmpty) "" else sectionsString + "."
      }
    }
  }


  def value: Parser[Any] = number | string | stringList | trueFalse
  def number = numberToken ^^ { x => if (x.contains('.')) x else x.toLong }
  def string = stringToken ^^ { s => attr.interpolate(prefix, s.substring(1, s.length - 1).unquoteC) }
  def stringList = "[" ~> repsep(string | numberToken, opt(",")) <~ (opt(",") ~ "]") ^^ { list => list.toArray }
  def trueFalse: Parser[Boolean] = ("(true|on)".r ^^ { x => true }) | ("(false|off)".r ^^ { x => false })


  def parse(in: String): Unit = {
    parseAll(root, in) match {
      case Success(result, _) => result
      case x @ Failure(msg, z) => throw new ParseException(x.toString)
      case x @ Error(msg, _) => throw new ParseException(x.toString)
    }
  }
}
