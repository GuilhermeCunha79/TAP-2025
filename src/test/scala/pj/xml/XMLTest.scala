package pj.xml

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError.XMLError
import pj.domain.Result
import pj.xml.XML.*

class XMLTest extends AnyFunSuite:

  test("fromNode with existing child - Should return Right(Node)"):
    val xml = <parent>
      <child>value</child>
    </parent>
    assert(fromNode(xml, "child") == Right(<child>value</child>))

  test("fromNode with missing child - Should return Left"):
    val xml = <parent>
      <another>value</another>
    </parent>
    assert(fromNode(xml, "child").isLeft)

  test("fromAttribute with existing attribute - Should return Right(value)"):
    val xml = <node attr="value"/>
    assert(fromAttribute(xml, "attr") == Right("value"))

  test("fromAttribute with missing attribute - Should return Left"):
    val xml = <node/>
    assert(fromAttribute(xml, "attr").isLeft)

  test("traverse with all successful transformations - Should return Right(List)"):
    val data = Seq("a", "b")
    val f: String => Result[Int] = s => Right(s.length)
    assert(traverse(data, f) == Right(List(1, 1)))

  test("traverse with one failing transformation - Should return Left"):
    val data = Seq("a", "")
    val f: String => Result[Int] = s => if s.isEmpty then Left(XMLError("empty")) else Right(s.length)
    assert(traverse(data, f).isLeft)
