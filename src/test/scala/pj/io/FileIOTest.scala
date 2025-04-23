package pj.io

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError.IOFileProblem

import java.io.{File, PrintWriter}

class FileIOTest extends AnyFunSuite:

  val testFilesDir = "files/group/ms01/"

  test("loadError deve carregar o arquivo emptyMessageAttribute_outError.xml corretamente"):
    val result = FileIO.loadError(testFilesDir + "emptyMessageAttribute_outError.xml")
    assert(result.isRight)
    assert(result.getOrElse("") == "InvalidTime(0)")

  test("loadError deve carregar o arquivo noMessageAttribute_outError.xml corretamente"):
    val result = FileIO.loadError(testFilesDir + "noMessageAttribute_outError.xml")
    assert(result.isRight)
    assert(result.getOrElse("") == "InvalidTime(-5)")

  test("load should return Right(Elem) when XML file exists and is valid"):
    val file = File.createTempFile("test", ".xml")
    val writer = new PrintWriter(file)
    writer.write("<root><child>content</child></root>")
    writer.close()

    val result = FileIO.load(file.getAbsolutePath)
    assert(result.isRight)

    file.delete()

  test("load should return Left(IOFileProblem) when file does not exist"):
    val result = FileIO.load("non_existent_file.xml")
    result match
      case Left(_: IOFileProblem) => succeed
      case _ => fail("Expected IOFileProblem")

  test("loadError returns Right(message) when XML has 'message' attribute"):
    val file = File.createTempFile("valid", ".xml")
    val writer = new PrintWriter(file)
    writer.write("""<root message="Something went wrong"/>""")
    writer.close()

    val result = FileIO.loadError(file.getAbsolutePath)
    assert(result == Right("Something went wrong"))

    file.delete()

  test("loadError returns Left(IOFileProblem) when XML has no 'message' attribute"):
    val file = File.createTempFile("invalid", ".xml")
    val writer = new PrintWriter(file)
    writer.write("""<root/>""")
    writer.close()

    val result = FileIO.loadError(file.getAbsolutePath)
    assert(result match
      case Left(IOFileProblem(msg)) => msg.contains("does not have message")
      case _ => false
    )

    file.delete()

  test("loadError returns Left(IOFileProblem) when file does not exist"):
    val result = FileIO.loadError("nonexistent.xml")
    assert(result match
      case Left(IOFileProblem(_)) => true
      case _ => false
    )
