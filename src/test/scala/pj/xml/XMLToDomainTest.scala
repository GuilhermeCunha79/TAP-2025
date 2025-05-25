package pj.xml

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Assertions.fail

import pj.domain.DomainError.*
import pj.domain.resources.Types.*
import pj.domain.resources.{HumanResource, Order, PhysicalResource, Product, Task}
import pj.domain.{DomainError, Result}

class XMLToDomainTest extends AnyFunSuite:

  // Helper to get a valid opaque type instance
  private def getValid[A](result: Result[A]): A =
    result.getOrElse(fail(s"Failed to create valid test data: $result"))


  test("getPhysicalResource with valid data - Should be valid"):
    val node = <Physical id="PRS_1" type="PRST 1"/>

    val maybePhysicalResource = for {
      id <- PhysicalResourceId.from("PRS_1")
      prType <- PhysicalResourceType.from("PRST 1")
    } yield PhysicalResource(id, prType)

    assert(XMLToDomain.getPhysicalResource(node) == maybePhysicalResource)


  test("getPhysicalResource with invalid physical id - Should not be valid"):
    val node = <Physical id="PRSS_1" type="PRST 1"/>

    assert(XMLToDomain.getPhysicalResource(node) == Left(DomainError.InvalidPhysicalId("PRSS_1")))


  test("getHumanResource with invalid physical resource type - Should not be valid"):
    val node = <Human id="HRS_1" name="Antonio">
      <Handles type="PRST 1"/>
      <Handles type="PRST 2"/>
    </Human>

    val physicalTypes = List(getValid(PhysicalResourceType.from("PRST 1")), getValid(PhysicalResourceType.from("PRST 4")))

    assert(XMLToDomain.getHumanResource(physicalTypes)(node) == Left(DomainError.PhysicalResourceTypeNotFound("HRS_1","PRST 2")))


  test("getHumanResource with valid data - Should be valid"):
    val node = <Human id="HRS_1" name="Antonio">
      <Handles type="PRST 1"/>
      <Handles type="PRST 2"/>
    </Human>

    val physicalTypes = List(getValid(PhysicalResourceType.from("PRST 1")), getValid(PhysicalResourceType.from("PRST 2")))

    val humanResourceValid = for {
      id <- HumanResourceId.from("HRS_1")
      name <- HumanResourceName.from("Antonio")
    } yield HumanResource(id, name, physicalTypes)

    assert(XMLToDomain.getHumanResource(physicalTypes)(node) == humanResourceValid)



  test("getOrder with valid data - Should be valid"):
    val node = <Order id="ORD_1" prdref="PRD_1" quantity="1"/>

    val validProductId = getValid(ProductId.from("PRD_1"))
    val validProductName = getValid(ProductName.from("Caixa"))
    val validTaskId = getValid(TaskId.from("TSK_1"))
    val validProduct = Product(validProductId, validProductName, List(validTaskId))
    val validProducts = List(validProduct)

    val orderValid = for {
      id <- OrderId.from("ORD_1")
      qtd <- OrderQuantity.from("1")
      prodId <- ProductId.from("PRD_1")
    } yield Order(id, qtd, prodId)

    assert(XMLToDomain.getOrder(validProducts)(node) == orderValid)


  test("getOrder with invalid product id - Should not be valid"):
    val node = <Order id="ORD_1" prdref="PRD_2" quantity="1"/>

    val validProductId = getValid(ProductId.from("PRD_1"))
    val validProductName = getValid(ProductName.from("Caixa"))
    val validTaskId = getValid(TaskId.from("TSK_1"))
    val validProduct = Product(validProductId, validProductName, List(validTaskId))
    val validProducts = List(validProduct)

    assert(XMLToDomain.getOrder(validProducts)(node) == Left(ProductDoesNotExist("PRD_2")))


  test("getTask with valid data - Should be valid"):
    val node = <Task id="TSK_1" time="100">
      <PhysicalResource type="PRST 1"/>
      <PhysicalResource type="PRST 2"/>
    </Task>

    val physicalTypes = List(getValid(PhysicalResourceType.from("PRST 1")), getValid(PhysicalResourceType.from("PRST 2")), getValid(PhysicalResourceType.from("PRST 3")))

    val expectedTask = for {
      taskId <- TaskId.from("TSK_1")
      taskTime <- TaskTime.from("100")
      prt1 <- PhysicalResourceType.from("PRST 1")
      prt2 <- PhysicalResourceType.from("PRST 2")
    } yield Task(taskId, taskTime, List(prt1, prt2))

    assert(XMLToDomain.getTask(physicalTypes)(node) == expectedTask)


  test("getTask with invalid task id - Should not be valid"):
    val node = <Task id="INVALID_ID" time="100">
      <PhysicalResource type="PRST 1"/>
      <PhysicalResource type="PRST 2"/>
    </Task>

    val physicalTypes = List(getValid(PhysicalResourceType.from("PRST 1")), getValid(PhysicalResourceType.from("PRST 2")), getValid(PhysicalResourceType.from("PRST 3")))

    assert(XMLToDomain.getTask(physicalTypes)(node) == Left(InvalidTaskId("INVALID_ID")))


  test("getTask with invalid time value - Should not be valid"):
    val node = <Task id="TSK_1" time="0">
      <PhysicalResource type="PRST 1"/>
      <PhysicalResource type="PRST 2"/>
    </Task>

    val physicalTypes = List(getValid(PhysicalResourceType.from("PRST 1")), getValid(PhysicalResourceType.from("PRST 2")), getValid(PhysicalResourceType.from("PRST 3")))

    assert(XMLToDomain.getTask(physicalTypes)(node) == Left(InvalidTime("0")))


  test("getTask with negative time value - Should not be valid"):
    val node = <Task id="TSK_1" time="-10">
      <PhysicalResource type="PRST 1"/>
      <PhysicalResource type="PRST 2"/>
    </Task>

    val physicalTypes = List(getValid(PhysicalResourceType.from("PRST 1")), getValid(PhysicalResourceType.from("PRST 2")), getValid(PhysicalResourceType.from("PRST 3")))

    assert(XMLToDomain.getTask(physicalTypes)(node) == Left(InvalidTime("-10")))


  test("getTask with non-numeric time value - Should not be valid"):
    val node = <Task id="TSK_1" time="abc">
      <PhysicalResource type="PRST 1"/>
      <PhysicalResource type="PRST 2"/>
    </Task>

    val physicalTypes = List(getValid(PhysicalResourceType.from("PRST 1")), getValid(PhysicalResourceType.from("PRST 2")), getValid(PhysicalResourceType.from("PRST 3")))

    assert(XMLToDomain.getTask(physicalTypes)(node) == Left(InvalidTime("abc")))


  test("getTask with non-existent physical resource type - Should not be valid"):
    val node = <Task id="TSK_1" time="100">
      <PhysicalResource type="PRST 1"/>
      <PhysicalResource type="NONEXISTENT"/>
    </Task>

    val physicalTypes = List(getValid(PhysicalResourceType.from("PRST 1")), getValid(PhysicalResourceType.from("PRST 2")), getValid(PhysicalResourceType.from("PRST 3")))

    assert(XMLToDomain.getTask(physicalTypes)(node) == Left(TaskUsesNonExistentPRT("NONEXISTENT")))


  test("getProduct with valid data - Should be valid"):
    val node = <Product id="PRD_1" name="Product 1">
      <Process tskref="TSK_1"/>
      <Process tskref="TSK_2"/>
    </Product>

    val task1Id = getValid(TaskId.from("TSK_1"))
    val task1Time = getValid(TaskTime.from("100"))
    val prt1 = getValid(PhysicalResourceType.from("PRST 1"))
    val task1: Task = Task(task1Id, task1Time, List(prt1))

    val task2Id = getValid(TaskId.from("TSK_2"))
    val task2Time = getValid(TaskTime.from("150"))
    val prt2 = getValid(PhysicalResourceType.from("PRST 2"))
    val task2: Task = Task(task2Id, task2Time, List(prt2))

    val tasks = List(task1, task2)

    val expectedProduct = for {
      productId <- ProductId.from("PRD_1")
      productName <- ProductName.from("Product 1")
    } yield Product(productId, productName, List(task1.id, task2.id))

    assert(XMLToDomain.getProduct(tasks)(node) == expectedProduct)


  test("getProduct with invalid product id - Should not be valid"):
    val node = <Product id="INVALID_ID" name="Product 1">
      <Process tskref="TSK_1"/>
    </Product>

    val task1Id = getValid(TaskId.from("TSK_1"))
    val task1Time = getValid(TaskTime.from("100"))
    val prt1 = getValid(PhysicalResourceType.from("PRST 1"))
    val task1: Task = Task(task1Id, task1Time, List(prt1))
    val tasks = List(task1)

    assert(XMLToDomain.getProduct(tasks)(node) == Left(InvalidProductId("INVALID_ID")))


  test("getProduct with non-existent task reference - Should not be valid"):
    val node = <Product id="PRD_1" name="Product 1">
      <Process tskref="TSK_1"/>
      <Process tskref="TSK_NONEXISTENT"/>
    </Product>

    val task1Id = getValid(TaskId.from("TSK_1"))
    val task1Time = getValid(TaskTime.from("100"))
    val prt1 = getValid(PhysicalResourceType.from("PRST 1"))
    val task1: Task = Task(task1Id, task1Time, List(prt1))
    val tasks = List(task1)

    assert(XMLToDomain.getProduct(tasks)(node) == Left(TaskDoesNotExist("TSK_NONEXISTENT")))


  test("getProduct with empty task list - Should be valid"):
    val node = <Product id="PRD_1" name="Simple Product">
    </Product>

    val tasks = List.empty[Task]

    val expectedProduct = for {
      productId <- ProductId.from("PRD_1")
      productName <- ProductName.from("Simple Product")
    } yield Product(productId, productName, List.empty)

    assert(XMLToDomain.getProduct(tasks)(node) == expectedProduct)


  test("getProduct with long name - Should be valid"):
    val node = <Product id="PRD_1" name="This is a very long product name that should still be valid">
      <Process tskref="TSK_1"/>
    </Product>

    val task1Id = getValid(TaskId.from("TSK_1"))
    val task1Time = getValid(TaskTime.from("100"))
    val prt1 = getValid(PhysicalResourceType.from("PRST 1"))
    val task1: Task = Task(task1Id, task1Time, List(prt1))
    val tasks = List(task1)

    val expectedProduct = for {
      productId <- ProductId.from("PRD_1")
      productName <- ProductName.from("This is a very long product name that should still be valid")
    } yield Product(productId, productName, List(task1.id))

    assert(XMLToDomain.getProduct(tasks)(node) == expectedProduct)