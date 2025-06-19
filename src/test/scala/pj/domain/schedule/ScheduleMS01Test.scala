package pj.domain.schedule

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError.*
import pj.domain.{DomainError, Result}
import pj.domain.resources.*
import pj.domain.resources.Types.{HumanResourceId, HumanResourceName, OrderId, OrderQuantity, PhysicalResourceId, PhysicalResourceType, ProductId, ProductName, TaskId, TaskTime}
import pj.domain.schedule.ScheduleMS01.generateSchedule
import pj.io.FileIO

import scala.language.adhocExtensions
import scala.xml.{Elem, XML}

class ScheduleMS01Test extends AnyFunSuite:

  test("scheduleDataRetriever"):
    val result: Result[(
      List[Order],
      List[Product],
      List[Task],
      List[HumanResource],
      List[PhysicalResource]
      )] = for {
      physicalResources <- for {
        id1 <- PhysicalResourceId.from("PRS_1")
        id2 <- PhysicalResourceId.from("PRS_2")
        id3 <- PhysicalResourceId.from("PRS_3")
        id4 <- PhysicalResourceId.from("PRS_4")
        id5 <- PhysicalResourceId.from("PRS_5")
        prt1 <- PhysicalResourceType.from("PRST 1")
        prt2 <- PhysicalResourceType.from("PRST 2")
        prt3 <- PhysicalResourceType.from("PRST 3")
        prt4 <- PhysicalResourceType.from("PRST 4")
        prt5 <- PhysicalResourceType.from("PRST 5")
      } yield List(
        PhysicalResource(id1, prt1),
        PhysicalResource(id2, prt2),
        PhysicalResource(id3, prt3),
        PhysicalResource(id4, prt4),
        PhysicalResource(id5, prt5)
      )

      physicalTypes <- Right(List("PRST 1", "PRST 2", "PRST 3", "PRST 4", "PRST 5"))

      tasks <- for {
        task1Id <- TaskId.from("TSK_1")
        task2Id <- TaskId.from("TSK_2")
        task1Time <- TaskTime.from("100")
        task2Time <- TaskTime.from("80")
        prt1 <- PhysicalResourceType.from("PRST 1")
        prt2 <- PhysicalResourceType.from("PRST 2")
        prt4 <- PhysicalResourceType.from("PRST 4")
        prt5 <- PhysicalResourceType.from("PRST 5")
      } yield List(
        Task(task1Id, task1Time, List(prt1, prt4)),
        Task(task2Id, task2Time, List(prt2, prt5))
      )

      humanResources <- for {
        hr1 <- HumanResourceId.from("HRS_1")
        hr2 <- HumanResourceId.from("HRS_2")
        hrName1 <- HumanResourceName.from("Antonio")
        hrName2 <- HumanResourceName.from("Maria")
        prt1 <- PhysicalResourceType.from("PRST 1")
        prt2 <- PhysicalResourceType.from("PRST 2")
        prt3 <- PhysicalResourceType.from("PRST 3")
        prt4 <- PhysicalResourceType.from("PRST 4")
        prt5 <- PhysicalResourceType.from("PRST 5")
      } yield List(
        HumanResource(hr1, hrName1, List(prt1, prt2)),
        HumanResource(hr2, hrName2, List(prt3, prt4, prt5))
      )

      products <- for {
        prod1 <- ProductId.from("PRD_1")
        prod2 <- ProductId.from("PRD_2")
        prodName1 <- ProductName.from("Product 1")
        prodName2 <- ProductName.from("Product 2")
        task1Id <- TaskId.from("TSK_1")
        task2Id <- TaskId.from("TSK_2")
      } yield List(
        Product(prod1, prodName1, List(task1Id, task2Id)),
        Product(prod2, prodName2, List(task2Id))
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
      orders,
      products,
      tasks,
      humanResources,
      physicalResources
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

  test("allocatePhysicalResources should allocate matching and available resources"):

    val result = for {
      taskId <- TaskId.from("TSK_1")
      id1 <- PhysicalResourceId.from("PRS_1")
      id2 <- PhysicalResourceId.from("PRS_2")
      id3 <- PhysicalResourceId.from("PRS_3")
      prtPrinter <- PhysicalResourceType.from("printer")
      prtScanner <- PhysicalResourceType.from("scanner")
    } yield
      val required = List(prtPrinter, prtScanner)
      val resources = List(
        PhysicalResource(id1, prtPrinter),
        PhysicalResource(id2, prtScanner),
        PhysicalResource(id3, prtPrinter)
      )
      ScheduleMS01.allocatePhysicalResources(taskId, required, resources)

    assert(result == Right(Right(List("PRS_1", "PRS_2"))))

  test("allocatePhysicalResources should fail if a required resource type is missing"):

    val result = for {
      taskId <- TaskId.from("TSK_2")
      id1 <- PhysicalResourceId.from("PRS_1")
      id2 <- PhysicalResourceId.from("PRS_2")
      prt1 <- PhysicalResourceType.from("printer")
      prt2 <- PhysicalResourceType.from("scanner")
      missing <- PhysicalResourceType.from("fax")
    } yield
      val required = List(prt1, prt2, missing)
      val resources = List(
        PhysicalResource(id1, prt1),
        PhysicalResource(id2, prt2)
      )
      ScheduleMS01.allocatePhysicalResources(taskId, required, resources)

    assert(result == Right(Left(DomainError.ResourceUnavailable("TSK_2", "fax"))))


  test("allocatePhysicalResources should not reuse the same resource for multiple required types"):

    val result = for {
      taskId <- TaskId.from("TSK_3")
      sharedId <- PhysicalResourceId.from("PRS_SHARED")
      prt1 <- PhysicalResourceType.from("printer")
      prt2 <- PhysicalResourceType.from("scanner")
    } yield
      val required = List(prt1, prt2)
      val resources = List(
        PhysicalResource(sharedId, prt1),
        PhysicalResource(sharedId, prt2)
      )
      ScheduleMS01.allocatePhysicalResources(taskId, required, resources)

    assert(result == Right(Left(DomainError.ResourceUnavailable("TSK_3", "scanner"))))


  test("allocatePhysicalResources should return empty list when no types are required"):

    val result = for {
      taskId <- TaskId.from("TSK_4")
      id1 <- PhysicalResourceId.from("PRS_1")
      prt1 <- PhysicalResourceType.from("printer")
    } yield
      val required = Nil
      val resources = List(PhysicalResource(id1, prt1))
      ScheduleMS01.allocatePhysicalResources(taskId, required, resources)

    assert(result == Right(Right(Nil)))


  test("allocatePhysicalResources should allocate based on resource order"):

    val result = for {
      taskId <- TaskId.from("TSK_5")
      id1 <- PhysicalResourceId.from("PRS_1")
      id2 <- PhysicalResourceId.from("PRS_2")
      prt1 <- PhysicalResourceType.from("printer")
      prt2 <- PhysicalResourceType.from("scanner")
    } yield
      val required = List(prt2, prt1)
      val resources = List(
        PhysicalResource(id1, prt1),
        PhysicalResource(id2, prt2)
      )
      ScheduleMS01.allocatePhysicalResources(taskId, required, resources)

    assert(result == Right(Right(List("PRS_2", "PRS_1"))))

  test("allocateHumanResources should allocate matching and available humans"):

    val result = for
      taskId <- TaskId.from("TSK_1")
      id1 <- HumanResourceId.from("HRS_1")
      id2 <- HumanResourceId.from("HRS_2")
      id3 <- HumanResourceId.from("HRS_3")
      name1 <- HumanResourceName.from("Alice")
      name2 <- HumanResourceName.from("Bob")
      name3 <- HumanResourceName.from("Charlie")
      prt <- PhysicalResourceType.from("printer")
      scn <- PhysicalResourceType.from("scanner")
    yield
      val required = List(prt, scn)
      val humans = List(
        HumanResource(id1, name1, List(prt)),
        HumanResource(id2, name2, List(scn)),
        HumanResource(id3, name3, List(prt))
      )
      ScheduleMS01.allocateHumanResources(taskId, required, humans)

    val expected = Right(Right(List("HRS_1", "HRS_2")))

    assert(result == expected)


  test("allocateHumanResources should fail if a required resource type is missing"):

    val result = for {
      taskId <- TaskId.from("TSK_2")
      id1 <- HumanResourceId.from("HRS_1")
      id2 <- HumanResourceId.from("HRS_2")
      name1 <- HumanResourceName.from("Alice")
      name2 <- HumanResourceName.from("Bob")
      prt <- PhysicalResourceType.from("printer")
      scn <- PhysicalResourceType.from("scanner")
      fax <- PhysicalResourceType.from("fax")
    } yield
      val required = List(prt, scn, fax)
      val humans = List(
        HumanResource(id1, name1, List(prt)),
        HumanResource(id2, name2, List(scn))
      )
      ScheduleMS01.allocateHumanResources(taskId, required, humans)

    assert(result == Right(Left(DomainError.ResourceUnavailable("TSK_2", "fax"))))

  test("allocateHumanResources should not reuse the same human for multiple required types"):

    val result = for {
      taskId <- TaskId.from("TSK_3")
      sharedId <- HumanResourceId.from("HRS_SHARED")
      name <- HumanResourceName.from("Alice")
      prt <- PhysicalResourceType.from("printer")
      scn <- PhysicalResourceType.from("scanner")
    } yield
      val required = List(prt, scn)
      val humans = List(
        HumanResource(sharedId, name, List(prt, scn))
      )
      ScheduleMS01.allocateHumanResources(taskId, required, humans)

    assert(result == Right(Left(DomainError.ResourceUnavailable("TSK_3", "scanner"))))

  test("allocateHumanResources should return empty list when no types are required"):

    val result = for {
      taskId <- TaskId.from("TSK_4")
      id1 <- HumanResourceId.from("HRS_1")
      name1 <- HumanResourceName.from("Alice")
      prt <- PhysicalResourceType.from("printer")
    } yield
      val required = Nil
      val humans = List(HumanResource(id1, name1, List(prt)))
      ScheduleMS01.allocateHumanResources(taskId, required, humans)

    assert(result == Right(Right(Nil)))


  test("allocateHumanResources should allocate based on human order"):

    val result = for {
      taskId <- TaskId.from("TSK_5")
      id1 <- HumanResourceId.from("HRS_1")
      id2 <- HumanResourceId.from("HRS_2")
      name1 <- HumanResourceName.from("Alice")
      name2 <- HumanResourceName.from("Bob")
      prt <- PhysicalResourceType.from("printer")
      scn <- PhysicalResourceType.from("scanner")
    } yield
      val required = List(scn, prt)
      val humans = List(
        HumanResource(id1, name1, List(prt)),
        HumanResource(id2, name2, List(scn))
      )
      ScheduleMS01.allocateHumanResources(taskId, required, humans)

    assert(result == Right(Right(List("HRS_2", "HRS_1"))))

  test("generateSchedule should succeed with minimal valid input"):
    val prt = PhysicalResourceType.from("printer")
    val prsId = PhysicalResourceId.from("PRS_1")
    val hrsId = HumanResourceId.from("HRS_1")
    val hrsName = HumanResourceName.from("Alice")
    val tskId = TaskId.from("TSK_1")
    val taskTime = TaskTime.from("2")
    val prodId = ProductId.from("PRD_1")
    val prodName = ProductName.from("Widget")
    val ordId = OrderId.from("ORD_1")
    val quantity = OrderQuantity.from("1")

    val result = for {
      p <- prt
      pId <- prsId
      hId <- hrsId
      hName <- hrsName
      tId <- tskId
      tTime <- taskTime
      prdId <- prodId
      prdName <- prodName
      oId <- ordId
      qty <- quantity

      task = Task(tId, tTime, List(p))
      product = Product(prdId, prdName, List(tId))
      order = Order(oId, qty, prdId)
      physicalResources = List(PhysicalResource(pId, p))
      humanResources = List(HumanResource(hId, hName, List(p)))

      xmlResult = generateSchedule(List(order),List(product), List(task),humanResources, physicalResources)
    } yield xmlResult

    result match
      case Right(Right(xml)) =>
        assert(xml.toString.contains("TSK_1")) // o XML contém o id da task

      case other =>
        fail(s"Expected success but got: $other")

  test("generateSchedule should fail if product does not exist"):
    val prt = PhysicalResourceType.from("printer")
    val pId = PhysicalResourceId.from("PRS_1")
    val hId = HumanResourceId.from("HRS_1")
    val hName = HumanResourceName.from("Bob")
    val tId = TaskId.from("TSK_1")
    val tTime = TaskTime.from("2")
    val oId = OrderId.from("ORD_1")
    val invalidProdId = ProductId.from("PRD_MISSING")
    val qty = OrderQuantity.from("1")

    val result = for {
      p <- prt
      pid <- pId
      hid <- hId
      hnm <- hName
      tid <- tId
      ttime <- tTime
      oid <- oId
      invProdId <- invalidProdId
      qtyVal <- qty

      task = Task(tid, ttime, List(p))
      order = Order(oid, qtyVal, invProdId)
      physicalResources = List(PhysicalResource(pid, p))
      humanResources = List(HumanResource(hid, hnm, List(p)))

      xmlResult = generateSchedule(List(order),List.empty, List(task),humanResources, physicalResources)
    } yield xmlResult

    result match
      case Right(Left(DomainError.ProductDoesNotExist(missing))) =>
        assert(missing == "PRD_MISSING")

      case other =>
        fail(s"Expected ProductDoesNotExist error but got: $other")

  test("scheduleOrder should fail if the product does not exist"):
    val oid = OrderId.from("ORD_1")
    val missingProdId = ProductId.from("PRD_404")
    val qty = OrderQuantity.from("1")
    val tid = TaskId.from("TSK_1")
    val ttime = TaskTime.from("2")
    val prt = PhysicalResourceType.from("printer")
    val prid = PhysicalResourceId.from("PRS_1")
    val hrid = HumanResourceId.from("HRS_1")
    val hrname = HumanResourceName.from("Bob")

    val result = for {
      id <- oid
      pid <- missingProdId
      q <- qty
      t <- tid
      time <- ttime
      p <- prt
      prid <- prid
      hrid <- hrid
      hrname <- hrname

      order = Order(id, q, pid)
      physicalResources = List(PhysicalResource(prid, p))
      humanResources = List(HumanResource(hrid, hrname, List(p)))

      scheduled = ScheduleMS01.scheduleOrder(
        order,
        startTime = 0,
        scheduled = Nil,
        allProducts = Nil,
        allTasks = List(Task(t, time, List(p))),
        physicalResources,
        humanResources
      )
    } yield scheduled

    result match
      case Right(Left(DomainError.ProductDoesNotExist(p))) =>
        assert(p == "PRD_404")
      case other =>
        fail(s"Expected ProductDoesNotExist error, but got $other")

  test("scheduleProduct should fail if a task does not exist"):
    val prodId = ProductId.from("PRD_1")
    val orderId = OrderId.from("ORD_1")
    val tid = TaskId.from("TSK_MISSING")
    val prt = PhysicalResourceType.from("printer")
    val prid = PhysicalResourceId.from("PRS_1")
    val hrid = HumanResourceId.from("HRS_1")
    val hrname = HumanResourceName.from("Bob")

    val result = for {
      pid <- prodId
      pname <- ProductName.from("MyProduct")
      oid <- orderId
      p <- prt
      prid <- prid
      hrid <- hrid
      hrname <- hrname
      tskId <- tid

      product = Product(pid, pname, List(tskId))
      physicalResources = List(PhysicalResource(prid, p))
      humanResources = List(HumanResource(hrid, hrname, List(p)))

      scheduled = ScheduleMS01.scheduleProduct(
        oid,
        product,
        productInstance = 1,
        startTime = 0,
        scheduled = Nil,
        allTasks = Nil,
        physicalResources,
        humanResources
      )
    } yield scheduled

    result match
      case Right(Left(DomainError.TaskDoesNotExist(t))) =>
        assert(t == "TSK_MISSING")
      case other =>
        fail(s"Expected TaskDoesNotExist error, but got $other")

  test("scheduleTask should schedule successfully with matching physical and human resources") {
    val tid = TaskId.from("TSK_1")
    val ttime = TaskTime.from("3")
    val orderId = OrderId.from("ORD_1")
    val prt = PhysicalResourceType.from("printer")
    val prid = PhysicalResourceId.from("PRS_1")
    val hrid = HumanResourceId.from("HRS_1")
    val hrname = HumanResourceName.from("Alice")

    val result = for {
      tskId <- tid
      time <- ttime
      p <- prt
      prid <- prid
      hrid <- hrid
      hrname <- hrname
      oid <- orderId

      task = Task(tskId, time, List(p))
      physicalResources = List(PhysicalResource(prid, p))
      humanResources = List(HumanResource(hrid, hrname, List(p)))

      scheduled <- ScheduleMS01.scheduleTask(
        oid,
        task,
        productInstance = 1,
        startTime = 0,
        physicalResources,
        humanResources
      )
    } yield scheduled

    result match
      case Right((taskSchedule, endTime)) =>
        assert(taskSchedule.taskId.to == "TSK_1")
        assert(endTime == 3)
        assert(taskSchedule.physicalResourceIds == List("PRS_1"))
        assert(taskSchedule.humanResourceIds == List("HRS_1"))
      case other =>
        fail(s"Expected successful scheduling, but got $other")
  }