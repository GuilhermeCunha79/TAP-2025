package pj.assessment

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.Result
import pj.io.FileIO.{load, loadError}

import java.io.File
import scala.xml.{Elem, Node, Utility}

trait AssessmentBehaviours extends AnyFunSuite:
  val IN = "_in.xml"              // Input file termination
  val OUT = "_out.xml"            // Output file termination
  val OUTERROR = "_outError.xml"  // Error file termination

  private def transformationResult(ms: Elem => Result[Elem], f: File): Result[Elem] =
    for
      ixml <- load(f)     // load input file
      oxml <- ms(ixml)    // convert input file into output file
    yield oxml

  private def invalidTest(ms: Elem => Result[Elem], expectedErrorFile: File, inputFile: File): Either[String, (String, String)] =
    val tr: Result[Elem] = transformationResult(ms, inputFile)
    val er: Result[String] = loadError(expectedErrorFile)
    tr match
      case Right(_) => Left("Expected ERROR but algorithm produced XML ELEMENT for file " + inputFile.getName)
      case Left(de) => er match
        case Right(dem) => Right( (de.toString, dem) )
        case Left(der) => Left("ERROR in file " + expectedErrorFile.getName)

  private def validTest(ms: Elem => Result[Elem], expectedOutputFile: File, inputFile: File): Either[String, (Node, Node)] =
    val tr: Result[Elem] = transformationResult(ms, inputFile)
    val or: Result[Elem] = load(expectedOutputFile)
    tr match
      case Right(ix) => or match
        case Right(ox) => Right((Utility.trim(ix), Utility.trim(ox)))
        case Left(oe) => Left("ERROR in file " + expectedOutputFile.getName)
      case Left(de) => Left("Expected XML ELEMENT but algorithm produced ERROR " + de + " for file " + inputFile.getName)

  private def performOneValidTest(ms: Elem => Result[Elem])(ft: (File, File)): Int =
    ft match
      case (inputFile, expectedOutputFile) =>
        val r = validTest(ms, expectedOutputFile, inputFile)
        test("File " + inputFile.getName + " should be valid"):
          r match
            case Left(s) => fail(s)
            case Right((ix, ox)) => assert(ix === ox, "XML files do not match!")
        r match
          case Left(s) => 0
          case Right((ix, ox)) => if ix.equals(ox) then 1 else 0

  private def performOneInvalidTest(ms: Elem => Result[Elem])(ft: (File, File)  ): Int =
    ft match
      case (inputFile, expectedErrorFile) =>
        val r = invalidTest(ms, expectedErrorFile, inputFile)
        test("File " + inputFile.getName + " should NOT be valid"):
          r match
            case Left(s) => fail(s)
            case Right((de, dem)) => assert(de == dem, "ERROR messages do not match!")
        r match
          case Left(s) => 0
          case Right((de, dem)) => if de.equals(dem) then 1 else 0
  
  def performAllTests(ms: Elem => Result[Elem], path: String, milestone: String): Unit =
    val filesUnsorted = new File(path).listFiles
    if (filesUnsorted == null || filesUnsorted.isEmpty) then
      println(s"There were no files for $milestone!")
    else
      val files = filesUnsorted.sortBy(_.getName)
      val validFiles = files.filter(_.getName.endsWith(OUT))
      val invalidFiles = files.filter(_.getName.endsWith(OUTERROR))
      val validFileTuple = validFiles.flatMap( f => files.find(_.getName.equals(f.getName.replace(OUT,IN))).map((_, f)) )
      val invalidFileTuple = invalidFiles.flatMap( f => files.find(_.getName.equals(f.getName.replace(OUTERROR,IN))).map((_, f)) )
      if ( (validFiles.length > validFileTuple.length) || (invalidFiles.length > invalidFileTuple.length) ) fail("Result File without Input!!!")
      val numTests = validFiles.length + invalidFiles.length
      val passedTests = validFileTuple.map(performOneValidTest(ms)).sum + invalidFileTuple.map(performOneInvalidTest(ms)).sum
      val message: String = if (numTests == 0) s"There were no tests for $milestone!"
      else
        val ratio: Int = (passedTests * 100) / numTests
        s"Final score of $milestone: $passedTests / $numTests = $ratio"
      println(message)
