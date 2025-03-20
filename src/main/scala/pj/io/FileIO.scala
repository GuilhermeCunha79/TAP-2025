package pj.io

import pj.domain.*
import pj.domain.DomainError.*

import java.io.{File, PrintWriter}
import scala.util.Try
import scala.xml.{Elem, PrettyPrinter, XML}

object FileIO:

  /** load xml by file name
   *
   * @param fn file name
   * @return a xml element if file exists or a domain error
   */
  def load(fn: String): Result[Elem] =
    Try(XML.loadFile(fn)).fold(t => Left(IOFileProblem(t.getMessage)), xml => Right(xml))

  /** load xml by file
   *
   * @param f file
   * @return a xml element if file exists or a domain error
   */
  def load(f: File): Result[Elem] =
    Try(XML.loadFile(f)).fold(t => Left(IOFileProblem(t.getMessage)), xml => Right(xml))
  
  /** load xml error file by filename
   *
   * @param fname file name
   * @return the message string if it exists or a domain error
   */
  def loadError(fname: String): Result[String] =
    Try(XML.loadFile(fname)).fold(t => Left(IOFileProblem(t.getMessage)) , xml => {
      val message  = xml \@ "message"
      if (message.isEmpty) Left(IOFileProblem("File does not have message"))
      else Right(message)
    })
  
  /** load xml error file
   *
   * @param f file
   * @return the message string if it exists or a domain error
   */
  def loadError(f: File): Result[String] =
    Try(XML.loadFile(f)).fold(t => Left(IOFileProblem(t.getMessage)) , xml => {
      val message  = xml \@ "message"
      if (message.isEmpty) Left(IOFileProblem("File does not have message"))
      else Right(message)
    })    

  // save xml to filename
  private val normalizer = new PrettyPrinter(120, 4)

  /** save xml element fo filename
   * 
   * @param fname file name
   * @param xml xml element
   */
  def save(fname: String, xml: Elem): Unit =
    val prettyXml = normalizer.format(xml)
    new PrintWriter(fname) { println("<?xml version='1.0' encoding=\"UTF-8\"?>"); println(prettyXml); close() }
