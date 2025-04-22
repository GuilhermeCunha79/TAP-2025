package pj.domain.schedule

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError.*
import pj.domain.{DomainError, Result}
import pj.domain.resources.*
import pj.domain.resources.Types.*
import pj.io.FileIO

import scala.language.adhocExtensions

// TODO: Create the code to test a functional domain model for schedule creation.
//       create files in the files/test/ms01 folder
class ScheduleMS01Test extends AnyFunSuite:

  test("ProductId.from with invalid prefix"):
    val result = ProductId.from("PXD_123")
    assert(result == Left(InvalidProductId("PXD_123")))

  test("ProductId.from with empty string"):
    val result = ProductId.from("")
    assert(result == Left(InvalidProductId("")))

  test("ProductId.to returns the original string"):
    val result = ProductId.from("PRD_123")
    assert(result.fold(_ => false, prodId => prodId.to == "PRD_123"))

  test("ProductId.from with string without prefix"):
    val result = ProductId.from("12345")
    assert(result == Left(InvalidProductId("12345")))

  test("ProductId.from with minimum valid input"):
    val result = ProductId.from("PRD_1")
    assert(result.fold(_ => false, _ => true))

  test("ProductId.from with long valid input"):
    val result = ProductId.from("PRD_123_ABC_456_DEF")
    assert(result.fold(_ => false, id => id.to == "PRD_123_ABC_456_DEF"))

  test("OrderQuantity.from with non-numeric string"):
    val result = OrderQuantity.from("abc")
    assert(result == Left(InvalidQuantity("abc")))

  test("OrderQuantity.from with empty string"):
    val result = OrderQuantity.from("")
    assert(result == Left(InvalidQuantity("")))

  test("OrderQuantity.from with zero"):
    val result = OrderQuantity.from("0")
    assert(result == Left(InvalidQuantity("0")))

  test("OrderQuantity.to returns the parsed int"):
    val result = OrderQuantity.from("7")
    assert(result.fold(_ => false, q => q.to == 7))

  test("OrderId.from with invalid prefix"):
    val result = OrderId.from("ORDX_123")
    assert(result == Left(InvalidOrderId("ORDX_123")))

  test("OrderId.from with empty string"):
    val result = OrderId.from("")
    assert(result == Left(InvalidOrderId("")))

  test("OrderId.from with minimum valid input"):
    val result = OrderId.from("ORD_1")
    assert(result.fold(_ => false, _ => true))

  test("OrderId.from with long valid input"):
    val result = OrderId.from("ORD_123_456_789_ABC")
    assert(result.fold(_ => false, id => id.to == "ORD_123_456_789_ABC"))

  test("OrderId.to returns the original string"):
    val result = OrderId.from("ORD_123")
    assert(result.fold(_ => false, orderId => orderId.to == "ORD_123"))

  test("PhysicalResourceId.from with invalid prefix"):
    val result = PhysicalResourceId.from("PRSX_123")
    assert(result == Left(InvalidPhysicalId("PRSX_123")))

  test("PhysicalResourceId.from with empty string"):
    val result = PhysicalResourceId.from("")
    assert(result == Left(InvalidPhysicalId("")))

  test("PhysicalResourceId.from with minimum valid input"):
    val result = PhysicalResourceId.from("PRS_1")
    assert(result.fold(_ => false, _ => true))

  test("PhysicalResourceId.from with long valid input"):
    val result = PhysicalResourceId.from("PRS_123_456_789_ABC")
    assert(result.fold(_ => false, id => id.to == "PRS_123_456_789_ABC"))

  test("PhysicalResourceId.to returns the original string"):
    val result = PhysicalResourceId.from("PRS_123")
    assert(result.fold(_ => false, id => id.to == "PRS_123"))

  test("TaskTime.from with positive integer string"):
    val result = TaskTime.from("10")
    assert(result.fold(_ => false, time => time.to == 10))

  test("TaskTime.from with minimum valid input (1)"):
    val result = TaskTime.from("1")
    assert(result.fold(_ => false, _ => true))

  test("TaskTime.from with non-numeric string"):
    val result = TaskTime.from("abc")
    assert(result == Left(InvalidTime("abc")))

  test("TaskTime.from with empty string"):
    val result = TaskTime.from("")
    assert(result == Left(InvalidTime("")))

  test("TaskTime.from with zero"):
    val result = TaskTime.from("0")
    assert(result == Left(InvalidTime("0")))

  test("TaskTime.from with negative number"):
    val result = TaskTime.from("-5")
    assert(result == Left(InvalidTime("-5")))

  test("TaskTime.to returns the parsed int"):
    val result = TaskTime.from("15")
    assert(result.fold(_ => false, t => t.to == 15))

  test("TaskId.from with invalid prefix"):
    val result = TaskId.from("TSKX_123")
    assert(result == Left(InvalidTaskId("TSKX_123")))

  test("TaskId.from with empty string"):
    val result = TaskId.from("")
    assert(result == Left(InvalidTaskId("")))

  test("TaskId.from with minimum valid input"):
    val result = TaskId.from("TSK_1")
    assert(result.fold(_ => false, _ => true))

  test("TaskId.from with long valid input"):
    val result = TaskId.from("TSK_123_456_789_ABC")
    assert(result.fold(_ => false, id => id.to == "TSK_123_456_789_ABC"))

  test("TaskId.to returns the original string"):
    val result = TaskId.from("TSK_123")
    assert(result.fold(_ => false, taskId => taskId.to == "TSK_123"))

  test("HumanResourceId.from with invalid prefix"):
    val result = HumanResourceId.from("HRSX_123")
    assert(result == Left(InvalidHumanId("HRSX_123")))

  test("HumanResourceId.from with empty string"):
    val result = HumanResourceId.from("")
    assert(result == Left(InvalidHumanId("")))

  test("HumanResourceId.from with minimum valid input"):
    val result = HumanResourceId.from("HRS_1")
    assert(result.fold(_ => false, _ => true))

  
  
  
  
  
  
  
  // ISTO AQUI FICA NESTA CLASSE
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