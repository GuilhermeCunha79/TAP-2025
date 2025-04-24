package pj.domain.schedule

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError.*
import pj.domain.{DomainError, Result}
import pj.domain.resources.*
import pj.domain.resources.Types.*
import pj.io.FileIO

import scala.language.adhocExtensions
import scala.xml.{Elem, XML}

class ScheduleMS01Test extends AnyFunSuite:

  test("scheduleDataRetriever"):
    val result: Result[(
      List[PhysicalResource],
        List[String],
        List[Task],
        List[HumanResource],
        List[Product],
        List[Order]
      )] = for {
      physicalResources <- for {
        id1 <- PhysicalResourceId.from("PRS_1")
        id2 <- PhysicalResourceId.from("PRS_2")
        id3 <- PhysicalResourceId.from("PRS_3")
        id4 <- PhysicalResourceId.from("PRS_4")
        id5 <- PhysicalResourceId.from("PRS_5")
      } yield List(
        PhysicalResource(id1, "PRST 1"),
        PhysicalResource(id2, "PRST 2"),
        PhysicalResource(id3, "PRST 3"),
        PhysicalResource(id4, "PRST 4"),
        PhysicalResource(id5, "PRST 5")
      )

      physicalTypes <- Right(List("PRST 1", "PRST 2", "PRST 3", "PRST 4", "PRST 5"))

      tasks <- for {
        task1Id <- TaskId.from("TSK_1")
        task2Id <- TaskId.from("TSK_2")
        task1Time <- TaskTime.from("100")
        task2Time <- TaskTime.from("80")
      } yield List(
        Task(task1Id, task1Time, List("PRST 1", "PRST 4")),
        Task(task2Id, task2Time, List("PRST 2", "PRST 5"))
      )

      humanResources <- for {
        hr1 <- HumanResourceId.from("HRS_1")
        hr2 <- HumanResourceId.from("HRS_2")
      } yield List(
        HumanResource(hr1, "Antonio", List("PRST 1", "PRST 2")),
        HumanResource(hr2, "Maria", List("PRST 3", "PRST 4", "PRST 5"))
      )

      products <- for {
        prod1 <- ProductId.from("PRD_1")
        prod2 <- ProductId.from("PRD_2")
        task1Id <- TaskId.from("TSK_1")
        task2Id <- TaskId.from("TSK_2")
      } yield List(
        Product(prod1, "Product 1", List(task1Id, task2Id)),
        Product(prod2, "Product 2", List(task2Id))
      )

      orders <- for {
        ord1 <- OrderId.from("ORD_1")
        ord2 <- OrderId.from("ORD_2")
        qty1 <- OrderQuantity.from("1")
        qty2 <- OrderQuantity.from("2")
        prod1Id <- ProductId.from("PRD_1")
        prod2Id <- ProductId.from("PRD_2")
      } yield List(
        Order(ord1, qty1, prod1Id),
        Order(ord2, qty2, prod2Id)
      )
    } yield (
      physicalResources,
      physicalTypes,
      tasks,
      humanResources,
      products,
      orders
    )

    val filePath = "src/test/scala/files/test/ms01/validAgenda_01_in.xml"

    FileIO.load(filePath) match
      case Right(xml) =>
        assert(ScheduleMS01.scheduleDataRetriever(xml) == result)
      case Left(error) =>
        fail(s"Erro ao carregar o arquivo XML: $error")

  test("create with invalid task time - should return error"):
    val xmlString =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<Production>
        |  <PhysicalResources>
        |    <Physical id="PRS_1" type="PRST_1"/>
        |  </PhysicalResources>
        |  <Tasks>
        |    <Task id="TSK_1" time="0"> <!-- Invalid time -->
        |      <PhysicalResource type="PRST_1"/>
        |    </Task>
        |  </Tasks>    
        |  <HumanResources>
        |    <Human id="HRS_1" name="Worker1">
        |      <Handles type="PRST_1"/>
        |    </Human>
        |  </HumanResources>
        |  <Products>
        |    <Product id="PRD_1" name="Product1">
        |      <Process tskref="TSK_1"/>
        |    </Product>
        |  </Products>
        |  <Orders>
        |    <Order id="ORD_1" prdref="PRD_1" quantity="1"/>
        |  </Orders>
        |</Production>""".stripMargin
  
    val xml = XML.loadString(xmlString)
    val result = ScheduleMS01.create(xml)
  
    assert(result.isLeft)
    assert(result == Left(InvalidTime("0")))

  test("create with invalid order quantity - should return error"):
    val xmlString =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<Production>
        |  <PhysicalResources>
        |    <Physical id="PRS_1" type="PRST_1"/>
        |  </PhysicalResources>
        |  <Tasks>
        |    <Task id="TSK_1" time="10">
        |      <PhysicalResource type="PRST_1"/>
        |    </Task>
        |  </Tasks>    
        |  <HumanResources>
        |    <Human id="HRS_1" name="Worker1">
        |      <Handles type="PRST_1"/>
        |    </Human>
        |  </HumanResources>
        |  <Products>
        |    <Product id="PRD_1" name="Product1">
        |      <Process tskref="TSK_1"/>
        |    </Product>
        |  </Products>
        |  <Orders>
        |    <Order id="ORD_1" prdref="PRD_1" quantity="0"/> <!-- Invalid quantity -->
        |  </Orders>
        |</Production>""".stripMargin
  
    val xml = XML.loadString(xmlString)
    val result = ScheduleMS01.create(xml)
  
    assert(result.isLeft)
    assert(result == Left(InvalidQuantity("0")))