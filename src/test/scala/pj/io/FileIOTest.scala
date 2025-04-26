package pj.io

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError.IOFileProblem

import java.io.{File, PrintWriter}

class FileIOTest extends AnyFunSuite:

  val testFilesDir = "src/test/scala/files/test/ms01/"

  test("loadError with empty message attribute - Should return Right with 'InvalidTime(0)'"):
    val result = FileIO.loadError(testFilesDir + "emptyMessageAttribute_outError.xml")
    assert(result.isRight)
    assert(result.getOrElse("") == "InvalidTime(0)")

  test("loadError with missing message attribute - Should return Right with 'InvalidTime(-5)'"):
    val result = FileIO.loadError(testFilesDir + "noMessageAttribute_outError.xml")
    assert(result.isRight)
    assert(result.getOrElse("") == "InvalidTime(-5)")

  test("load with valid XML file - Should return Right(Elem)"):
    val file = File.createTempFile("test", ".xml")
    val writer = new PrintWriter(file)
    writer.write("<root><child>content</child></root>")
    writer.close()

    val result = FileIO.load(file.getAbsolutePath)
    assert(result.isRight)

    file.delete()

  test("load with non-existent file - Should return Left(IOFileProblem)"):
    val result = FileIO.load("non_existent_file.xml")
    assert(result.fold(_.toString.contains("IOFileProblem"), _ => false))

  test("loadError with valid message attribute - Should return Right(message)"):
    val file = File.createTempFile("valid", ".xml")
    val writer = new PrintWriter(file)
    writer.write("""<root message="Something went wrong"/>""")
    writer.close()

    val result = FileIO.loadError(file.getAbsolutePath)
    assert(result == Right("Something went wrong"))

    file.delete()

  test("loadError with missing message attribute - Should return Left with missing message error"):
    val file = File.createTempFile("invalid", ".xml")
    val writer = new PrintWriter(file)
    writer.write("""<root/>""")
    writer.close()

    val result = FileIO.loadError(file.getAbsolutePath)
    file.delete()

    assert(result.fold(_.toString.contains("does not have message"), _ => false))

  test("loadError with non-existent file - Should return Left(IOFileProblem)"):
    val result = FileIO.loadError("nonexistent.xml")
    assert(result.fold(_.toString.contains("IOFileProblem"), _ => false))