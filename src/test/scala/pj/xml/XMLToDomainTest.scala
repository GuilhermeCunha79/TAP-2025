package pj.xml

import org.scalatest.funsuite.AnyFunSuite

import pj.domain.DomainError.*
import pj.domain.resources.Types.*
import pj.domain.resources.{HumanResource, Order, PhysicalResource, Product, Task}
import pj.domain.{DomainError, Result}
import pj.xml.XMLToDomain

class XMLToDomainTest extends AnyFunSuite:
  //XMLToDomain

  //Physical Resources
  test("getPhysicalResource with valid data - Should be valid"):
    val node = <Physical id="PRS_1" type="PRST 1"/>

    val maybePhysicalResource = PhysicalResourceId.from("PRS_1").map(id => PhysicalResource(id, "PRST 1"))
    assert(XMLToDomain.getPhysicalResource(node).equals(maybePhysicalResource))

  test("getPhysicalResource with invalid physical id - Should not be valid"):
    val node = <Physical id="PRSS_1" type="PRST 1"/>

    assert(XMLToDomain.getPhysicalResource(node) == Left(DomainError.InvalidPhysicalId("PRSS_1")))

  //Human Resources
  test("getHumanResource with invalid physical resource type - Should not be valid"):
    val node = <Human id="HRS_1" name="Antonio">
      <Handles type="PRST 1"/>
      <Handles type="PRST 2"/>
    </Human>

    val physicalTypes = List("PRST 1", "PRST 4")

    assert(XMLToDomain.getHumanResource(physicalTypes)(node) == Left(DomainError.PhysicalResourceTypeNotFound("HRS_1", "PRST 2")))

  test("getHumanResource with valid data - Should be valid"):
    val node = <Human id="HRS_1" name="Antonio">
      <Handles type="PRST 1"/>
      <Handles type="PRST 2"/>
    </Human>

    val physicalTypes = List("PRST 1", "PRST 2")
    val humanResourceValid =
      HumanResourceId.from("HRS_1").map(id => HumanResource(id, "Antonio", physicalTypes))

    assert(XMLToDomain.getHumanResource(physicalTypes)(node) == humanResourceValid)

  //Orders
  test("getOrder with valid data - Should be valid"):
    val node = <Order id="ORD_1" prdref="PRD_1" quantity="1"/>

    val task: Result[Task] = for {
      tsk_id <- TaskId.from("TSK_1")
      tsk_time <- TaskTime.from("22")
    } yield Task(tsk_id, tsk_time, List("PRST 1"))

    val product: Either[DomainError, Product] = for {
      prod_id <- ProductId.from("PRD_1")
      taskId <- task.map(_.id)
    } yield Product(prod_id, "Caixa", List(taskId))

    val prodList = List(product)
    val validProducts = prodList.collect { case Right(p) => p }

    val orderValid = for {
      id <- OrderId.from("ORD_1")
      qtd <- OrderQuantity.from("1")
      prodId <- ProductId.from("PRD_1")
    } yield Order(id, qtd, prodId)

    assert(XMLToDomain.getOrder(validProducts)(node) == orderValid)

  test("getOrder with invalid product id - Should not be valid"):
    val node = <Order id="ORD_1" prdref="PRD_2" quantity="1"/>

    val task: Result[Task] = for {
      tsk_id <- TaskId.from("TSK_1")
      tsk_time <- TaskTime.from("22")
    } yield Task(tsk_id, tsk_time, List("PRST 1"))

    val product: Either[DomainError, Product] = for {
      prod_id <- ProductId.from("PRD_1")
      taskId <- task.map(_.id)
    } yield Product(prod_id, "Caixa", List(taskId))

    val prodList = List(product)
    val validProducts = prodList.collect { case Right(p) => p }

    val orderValid = for {
      id <- OrderId.from("ORD_1")
      qtd <- OrderQuantity.from("1")
      prodId <- ProductId.from("PRD_1")
    } yield Order(id, qtd, prodId)

    assert(XMLToDomain.getOrder(validProducts)(node) == Left(ProductDoesNotExist("PRD_2")))
    
    //Tasks
    
    //Products
