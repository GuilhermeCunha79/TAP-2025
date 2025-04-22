package pj.io

import org.scalatest.funsuite.AnyFunSuite

class FileIOErrorTest extends AnyFunSuite {
  
  val testFilesDir = "files/group/ms01/"
  
  test("loadError deve carregar o arquivo emptyMessageAttribute_outError.xml corretamente"):
    val result = FileIO.loadError(testFilesDir + "emptyMessageAttribute_outError.xml")
    assert(result.isRight)
    assert(result.getOrElse("") == "InvalidTime(0)")
  
  test("loadError deve carregar o arquivo noMessageAttribute_outError.xml corretamente"):
    val result = FileIO.loadError(testFilesDir + "noMessageAttribute_outError.xml")
    assert(result.isRight)
    assert(result.getOrElse("") == "InvalidTime(-5)")
} 