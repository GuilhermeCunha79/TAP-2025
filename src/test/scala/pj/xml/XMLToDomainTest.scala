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

    assert(XMLToDomain.getHumanResource(physicalTypes)(node) == Left(DomainError.PhysicalResourceTypeNotFound("HRS_1","PRST 2")))

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
  test("getTask with valid data - Should be valid"):
    val node = <Task id="TSK_1" time="100">
      <PhysicalResource type="PRST 1"/>
      <PhysicalResource type="PRST 2"/>
    </Task>

    val physicalTypes = List("PRST 1", "PRST 2", "PRST 3")
    
    val expectedTask = for {
      taskId <- TaskId.from("TSK_1")
      taskTime <- TaskTime.from("100")
    } yield Task(taskId, taskTime, List("PRST 1", "PRST 2"))
    
    assert(XMLToDomain.getTask(physicalTypes)(node) == expectedTask)
    
  test("getTask with invalid task id - Should not be valid"):
    val node = <Task id="INVALID_ID" time="100">
      <PhysicalResource type="PRST 1"/>
      <PhysicalResource type="PRST 2"/>
    </Task>
    
    val physicalTypes = List("PRST 1", "PRST 2", "PRST 3")
    
    assert(XMLToDomain.getTask(physicalTypes)(node) == Left(InvalidTaskId("INVALID_ID")))
    
  test("getTask with invalid time value - Should not be valid"):
    val node = <Task id="TSK_1" time="0">
      <PhysicalResource type="PRST 1"/>
      <PhysicalResource type="PRST 2"/>
    </Task>
    
    val physicalTypes = List("PRST 1", "PRST 2", "PRST 3")
    
    assert(XMLToDomain.getTask(physicalTypes)(node) == Left(InvalidTime("0")))
    
  test("getTask with negative time value - Should not be valid"):
    val node = <Task id="TSK_1" time="-10">
      <PhysicalResource type="PRST 1"/>
      <PhysicalResource type="PRST 2"/>
    </Task>
    
    val physicalTypes = List("PRST 1", "PRST 2", "PRST 3")
    
    assert(XMLToDomain.getTask(physicalTypes)(node) == Left(InvalidTime("-10")))
    
  test("getTask with non-numeric time value - Should not be valid"):
    val node = <Task id="TSK_1" time="abc">
      <PhysicalResource type="PRST 1"/>
      <PhysicalResource type="PRST 2"/>
    </Task>
    
    val physicalTypes = List("PRST 1", "PRST 2", "PRST 3")
    
    assert(XMLToDomain.getTask(physicalTypes)(node) == Left(InvalidTime("abc")))
    
  test("getTask with non-existent physical resource type - Should not be valid"):
    val node = <Task id="TSK_1" time="100">
      <PhysicalResource type="PRST 1"/>
      <PhysicalResource type="NONEXISTENT"/>
    </Task>
    
    val physicalTypes = List("PRST 1", "PRST 2", "PRST 3")
    
    assert(XMLToDomain.getTask(physicalTypes)(node) == Left(TaskUsesNonExistentPRT("NONEXISTENT")))
    
  //Products
  test("getProduct with valid data - Should be valid"):
    val node = <Product id="PRD_1" name="Product 1">
      <Process tskref="TSK_1"/>
      <Process tskref="TSK_2"/>
    </Product>
    
    val task1: Task = Task(TaskId.from("TSK_1").getOrElse(???), TaskTime.from("100").getOrElse(???), List("PRST 1"))
    val task2: Task = Task(TaskId.from("TSK_2").getOrElse(???), TaskTime.from("150").getOrElse(???), List("PRST 2"))
    val tasks = List(task1, task2)
    
    val expectedProduct = for {
      productId <- ProductId.from("PRD_1")
    } yield Product(productId, "Product 1", List(task1.id, task2.id))
    
    assert(XMLToDomain.getProduct(tasks)(node) == expectedProduct)
    
  test("getProduct with invalid product id - Should not be valid"):
    val node = <Product id="INVALID_ID" name="Product 1">
      <Process tskref="TSK_1"/>
    </Product>
    
    val task1: Task = Task(TaskId.from("TSK_1").getOrElse(???), TaskTime.from("100").getOrElse(???), List("PRST 1"))
    val tasks = List(task1)
    
    assert(XMLToDomain.getProduct(tasks)(node) == Left(InvalidProductId("INVALID_ID")))
    
  test("getProduct with non-existent task reference - Should not be valid"):
    val node = <Product id="PRD_1" name="Product 1">
      <Process tskref="TSK_1"/>
      <Process tskref="TSK_NONEXISTENT"/>
    </Product>
    
    val task1: Task = Task(TaskId.from("TSK_1").getOrElse(???), TaskTime.from("100").getOrElse(???), List("PRST 1"))
    val tasks = List(task1)
    
    assert(XMLToDomain.getProduct(tasks)(node) == Left(TaskDoesNotExist("TSK_NONEXISTENT")))
    
  test("getProduct with empty task list - Should be valid"):
    val node = <Product id="PRD_1" name="Simple Product">
    </Product>
    
    val tasks = List.empty[Task]
    
    val expectedProduct = for {
      productId <- ProductId.from("PRD_1")
    } yield Product(productId, "Simple Product", List.empty)
    
    assert(XMLToDomain.getProduct(tasks)(node) == expectedProduct)
    
  test("getProduct with long name - Should be valid"):
    val node = <Product id="PRD_1" name="This is a very long product name that should still be valid">
      <Process tskref="TSK_1"/>
    </Product>
    
    val task1: Task = Task(TaskId.from("TSK_1").getOrElse(???), TaskTime.from("100").getOrElse(???), List("PRST 1"))
    val tasks = List(task1)
    
    val expectedProduct = for {
      productId <- ProductId.from("PRD_1")
    } yield Product(productId, "This is a very long product name that should still be valid", List(task1.id))
    
    assert(XMLToDomain.getProduct(tasks)(node) == expectedProduct)
