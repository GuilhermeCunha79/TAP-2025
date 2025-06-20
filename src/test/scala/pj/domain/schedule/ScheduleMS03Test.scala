package pj.domain.schedule

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.{DomainError, Result}
import pj.domain.resources.Types.{EarliestStartTime, HumanResourceId, HumanResourceName, OrderId, OrderQuantity, PhysicalResourceId, PhysicalResourceType, ProductId, ProductName, ProductNumber, ProductTaskIndex, TaskId, TaskScheduleTime, TaskTime}
import pj.domain.resources.{HumanResource, Order, PhysicalResource, Product, Task, TaskInfo, TaskSchedule}
import pj.domain.schedule.ScheduleMS03.{createAllProductInstances, createTaskSchedule, generateSchedule, isMatchingTask, prioritizeTasks, tryScheduleTask, updateResourceAvailability, validateResourceRequirements}

import scala.xml.XML

class ScheduleMS03Test extends AnyFunSuite:
  private def createBasicTestData(): Result[(
    List[Order],
    List[Product],
    List[Task],
    List[HumanResource],
    List[PhysicalResource]
    )] = for {

    prs1 <- PhysicalResourceId.from("PRS_1")
    prs2 <- PhysicalResourceId.from("PRS_2")
    prs3 <- PhysicalResourceId.from("PRS_3")
    prt1 <- PhysicalResourceType.from("printer")
    prt2 <- PhysicalResourceType.from("scanner")
    prt3 <- PhysicalResourceType.from("cutter")
    physicalResources = List(
      PhysicalResource(prs1, prt1),
      PhysicalResource(prs2, prt2),
      PhysicalResource(prs3, prt3)
    )

    hrs1 <- HumanResourceId.from("HRS_1")
    hrs2 <- HumanResourceId.from("HRS_2")
    hrName1 <- HumanResourceName.from("Alice")
    hrName2 <- HumanResourceName.from("Bob")
    humanResources = List(
      HumanResource(hrs1, hrName1, List(prt1, prt2)),
      HumanResource(hrs2, hrName2, List(prt2, prt3))
    )

    tsk1 <- TaskId.from("TSK_1")
    tsk2 <- TaskId.from("TSK_2")
    time1 <- TaskTime.from("10")
    time2 <- TaskTime.from("15")
    tasks = List(
      Task(tsk1, time1, List(prt1)),
      Task(tsk2, time2, List(prt2))
    )

    prd1 <- ProductId.from("PRD_1")
    prdName1 <- ProductName.from("Product1")
    products = List(
      Product(prd1, prdName1, List(tsk1, tsk2))
    )

    ord1 <- OrderId.from("ORD_1")
    qty1 <- OrderQuantity.from("1")
    orders = List(
      Order(ord1, qty1, prd1)
    )
  } yield (orders, products, tasks, humanResources, physicalResources)

  test("validateResourceRequirements should succeed with sufficient resources"):
    createBasicTestData() match
      case Right((_, _, tasks, humanResources, physicalResources)) =>
        val result = validateResourceRequirements(tasks, physicalResources, humanResources)
        assert(result.isRight)
      case Left(error) =>
        fail(s"Failed to create test data: $error")

  test("validateResourceRequirements should fail with insufficient physical resources"):
    val result = for {
      tsk1 <- TaskId.from("TSK_1")
      time1 <- TaskTime.from("10")
      prt1 <- PhysicalResourceType.from("printer")
      task = Task(tsk1, time1, List(prt1, prt1))

      prs1 <- PhysicalResourceId.from("PRS_1")
      physicalResource = PhysicalResource(prs1, prt1)

      hrs1 <- HumanResourceId.from("HRS_1")
      hrName1 <- HumanResourceName.from("Alice")
      humanResource = HumanResource(hrs1, hrName1, List(prt1))

      validation = validateResourceRequirements(
        List(task),
        List(physicalResource),
        List(humanResource)
      )
    } yield validation

    result match
      case Right(Left(DomainError.ImpossibleSchedule)) =>
        assert(true)
      case other =>
        fail(s"Expected ImpossibleSchedule error but got: $other")

  test("validateResourceRequirements should fail when human cannot handle required resource type"):
    val result = for {
      tsk1 <- TaskId.from("TSK_1")
      time1 <- TaskTime.from("10")
      prt1 <- PhysicalResourceType.from("printer")
      prt2 <- PhysicalResourceType.from("scanner")
      task = Task(tsk1, time1, List(prt1))

      prs1 <- PhysicalResourceId.from("PRS_1")
      physicalResource = PhysicalResource(prs1, prt1)

      hrs1 <- HumanResourceId.from("HRS_1")
      hrName1 <- HumanResourceName.from("Alice")
      humanResource = HumanResource(hrs1, hrName1, List(prt2))

      validation = validateResourceRequirements(
        List(task),
        List(physicalResource),
        List(humanResource)
      )
    } yield validation

    result match
      case Right(Left(DomainError.ImpossibleSchedule)) =>
        assert(true)
      case other =>
        fail(s"Expected ImpossibleSchedule error but got: $other")

  test("createAllProductInstances should create correct number of task instances"):
    val result = for {
      ord1 <- OrderId.from("ORD_1")
      qty1 <- OrderQuantity.from("2")
      prd1 <- ProductId.from("PRD_1")
      order = Order(ord1, qty1, prd1)

      tsk1 <- TaskId.from("TSK_1")
      tsk2 <- TaskId.from("TSK_2")
      time1 <- TaskTime.from("10")
      time2 <- TaskTime.from("15")
      prt1 <- PhysicalResourceType.from("printer")
      task1 = Task(tsk1, time1, List(prt1))
      task2 = Task(tsk2, time2, List(prt1))

      productTaskMap = Map(prd1 -> List(tsk1, tsk2))
      taskMap = Map(tsk1 -> task1, tsk2 -> task2)

      instances = createAllProductInstances(List(order), productTaskMap, taskMap)
    } yield instances

    result match
      case Right(instances) =>
        assert(instances.lengthIs == 4)
        assert(instances.count(_.productNumber.to == 1) == 2)
        assert(instances.count(_.productNumber.to == 2) == 2)
        assert(instances.count(_.taskId.to == "TSK_1") == 2)
        assert(instances.count(_.taskId.to == "TSK_2") == 2)
      case Left(error) =>
        fail(s"Failed to create instances: $error")

  test("tryScheduleTask should successfully allocate resources"):
    val result = for {
      tsk1 <- TaskId.from("TSK_1")
      time1 <- TaskTime.from("10")
      prt1 <- PhysicalResourceType.from("printer")
      task = Task(tsk1, time1, List(prt1))

      ord1 <- OrderId.from("ORD_1")
      pn1 <- ProductNumber.from(1)
      est <- EarliestStartTime.from(0)
      pti <- ProductTaskIndex.from(0)
      taskInfo = TaskInfo(ord1, pn1, tsk1, task, est, pti)

      prs1 <- PhysicalResourceId.from("PRS_1")
      physicalResource = PhysicalResource(prs1, prt1)

      hrs1 <- HumanResourceId.from("HRS_1")
      hrName1 <- HumanResourceName.from("Alice")
      humanResource = HumanResource(hrs1, hrName1, List(prt1))

      allocation = tryScheduleTask(
        taskInfo,
        currentTime = 0,
        availablePhysical = List(physicalResource),
        availableHuman = List(humanResource),
        usedPhysicalIds = Set.empty,
        usedHumanIds = Set.empty
      )
    } yield allocation

    result match
      case Right(Right((physicalIds, humanIds))) =>
        assert(physicalIds.map(_.to) == List("PRS_1"))
        assert(humanIds.map(_.to) == List("HRS_1"))
      case other =>
        fail(s"Expected successful allocation but got: $other")

  test("tryScheduleTask should fail when resources are already used"):
    val result = for {
      tsk1 <- TaskId.from("TSK_1")
      time1 <- TaskTime.from("10")
      prt1 <- PhysicalResourceType.from("printer")
      task = Task(tsk1, time1, List(prt1))

      ord1 <- OrderId.from("ORD_1")
      pn1 <- ProductNumber.from(1)
      est <- EarliestStartTime.from(0)
      pti <- ProductTaskIndex.from(0)
      taskInfo = TaskInfo(ord1, pn1, tsk1, task, est, pti)

      prs1 <- PhysicalResourceId.from("PRS_1")
      physicalResource = PhysicalResource(prs1, prt1)

      hrs1 <- HumanResourceId.from("HRS_1")
      hrName1 <- HumanResourceName.from("Alice")
      humanResource = HumanResource(hrs1, hrName1, List(prt1))

      allocation = tryScheduleTask(
        taskInfo,
        currentTime = 0,
        availablePhysical = List(physicalResource),
        availableHuman = List(humanResource),
        usedPhysicalIds = Set(prs1),
        usedHumanIds = Set(hrs1)
      )
    } yield allocation

    result match
      case Right(Left(DomainError.ImpossibleSchedule)) =>
        assert(true)
      case other =>
        fail(s"Expected ImpossibleSchedule error but got: $other")

  test("createTaskSchedule should create valid TaskSchedule"):

    val result = for {
      tsk1 <- TaskId.from("TSK_1").toOption
      time1 <- TaskTime.from("10").toOption
      prt1 <- PhysicalResourceType.from("printer").toOption
      task = Task(tsk1, time1, List(prt1))

      ord1 <- OrderId.from("ORD_1").toOption
      pn1 <- ProductNumber.from(1).toOption
      est <- EarliestStartTime.from(0).toOption
      pti <- ProductTaskIndex.from(0).toOption
      taskInfo = TaskInfo(ord1, pn1, tsk1, task, est, pti)

      prs1 <- PhysicalResourceId.from("PRS_1").toOption
      hrs1 <- HumanResourceId.from("HRS_1").toOption

      scheduleResult = createTaskSchedule(taskInfo, 5, List(prs1), List(hrs1))
    } yield (scheduleResult, taskInfo)

    result match
      case Some((Right(schedule), taskInfo)) =>
        assert(schedule.orderId == taskInfo.orderId)
        assert(schedule.productNumber == taskInfo.productNumber)
        assert(schedule.taskId == taskInfo.taskId)
        assert(schedule.start.to == 5)
        assert(schedule.end.to == 15)
        assert(schedule.physicalResourceIds.map(_.to) == List("PRS_1"))
        assert(schedule.humanResourceIds.map(_.to) == List("HRS_1"))
      case Some((Left(error), _)) =>
        fail(s"Failed to create schedule: $error")
      case None =>
        fail("Failed to construct test data: invalid ID or value")

  test("isMatchingTask should correctly identify matching tasks"):
    val result = for {
      ord1 <- OrderId.from("ORD_1")
      ord2 <- OrderId.from("ORD_2")
      pn1 <- ProductNumber.from(1)
      pn2 <- ProductNumber.from(2)
      tsk1 <- TaskId.from("TSK_1")
      tsk2 <- TaskId.from("TSK_2")
      time1 <- TaskTime.from("10")
      prt1 <- PhysicalResourceType.from("printer")
      task = Task(tsk1, time1, List(prt1))
      est <- EarliestStartTime.from(0)
      pti <- ProductTaskIndex.from(0)

      taskInfo1 = TaskInfo(ord1, pn1, tsk1, task, est, pti)
      taskInfo2 = TaskInfo(ord1, pn1, tsk1, task, est, pti)
      taskInfo3 = TaskInfo(ord2, pn1, tsk1, task, est, pti)
      taskInfo4 = TaskInfo(ord1, pn2, tsk1, task, est, pti)
      taskInfo5 = TaskInfo(ord1, pn1, tsk2, task, est, pti)
    } yield (taskInfo1, taskInfo2, taskInfo3, taskInfo4, taskInfo5)

    result match
      case Right((t1, t2, t3, t4, t5)) =>
        assert(isMatchingTask(t1, t2))
        assert(!isMatchingTask(t1, t3))
        assert(!isMatchingTask(t1, t4))
        assert(!isMatchingTask(t1, t5))
      case Left(error) =>
        fail(s"Failed to create test tasks: $error")

  test("updateResourceAvailability should update all resource end times"):
    val result = for {
      ord1 <- OrderId.from("ORD_1")
      pn1 <- ProductNumber.from(1)
      tsk1 <- TaskId.from("TSK_1")
      start <- TaskScheduleTime.from(0)
      end <- TaskScheduleTime.from(10)
      prs1 <- PhysicalResourceId.from("PRS_1")
      hrs1 <- HumanResourceId.from("HRS_1")

      schedule = TaskSchedule(ord1, pn1, tsk1, start, end, List(prs1), List(hrs1))
      initialAvailability = Map("PRS_1" -> 0, "HRS_1" -> 0, "OTHER" -> 5)

      updated = updateResourceAvailability(initialAvailability, schedule, 20)
    } yield updated

    result match
      case Right(updated) =>
        assert(updated("PRS_1") == 20)
        assert(updated("HRS_1") == 20)
        assert(updated("OTHER") == 5)
      case Left(error) =>
        fail(s"Failed to update availability: $error")

  test("generateSchedule should succeed with valid minimal input"):
    createBasicTestData() match
      case Right((orders, products, tasks, humanResources, physicalResources)) =>
        val result = generateSchedule(orders, products, tasks, humanResources, physicalResources)

        result match
          case Right(schedules) =>
            assert(schedules.nonEmpty)
            assert(schedules.forall(_.orderId.to == "ORD_1"))
            assert(schedules.lengthIs == 2)
          case Left(error) =>
            fail(s"Expected successful schedule generation but got: $error")
      case Left(error) =>
        fail(s"Failed to create test data: $error")

  test("generateSchedule should fail with impossible resource requirements"):
    val result = for {
      tsk1 <- TaskId.from("TSK_1")
      time1 <- TaskTime.from("10")
      prt1 <- PhysicalResourceType.from("printer")
      prt2 <- PhysicalResourceType.from("unavailable_type")
      task = Task(tsk1, time1, List(prt2))

      prs1 <- PhysicalResourceId.from("PRS_1")
      physicalResource = PhysicalResource(prs1, prt1)

      hrs1 <- HumanResourceId.from("HRS_1")
      hrName1 <- HumanResourceName.from("Alice")
      humanResource = HumanResource(hrs1, hrName1, List(prt1))

      prd1 <- ProductId.from("PRD_1")
      prdName1 <- ProductName.from("Product1")
      product = Product(prd1, prdName1, List(tsk1))

      ord1 <- OrderId.from("ORD_1")
      qty1 <- OrderQuantity.from("1")
      order = Order(ord1, qty1, prd1)

      schedule = generateSchedule(
        List(order),
        List(product),
        List(task),
        List(humanResource),
        List(physicalResource)
      )
    } yield schedule

    result match
      case Right(Left(DomainError.ImpossibleSchedule)) =>
        assert(true)
      case other =>
        fail(s"Expected ImpossibleSchedule error but got: $other")

  test("create should successfully process XML input"):
    val xmlString =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<Production>
        |  <PhysicalResources>
        |    <Physical id="PRS_1" type="printer"/>
        |    <Physical id="PRS_2" type="scanner"/>
        |  </PhysicalResources>
        |  <Tasks>
        |    <Task id="TSK_1" time="10">
        |      <PhysicalResource type="printer"/>
        |    </Task>
        |    <Task id="TSK_2" time="15">
        |      <PhysicalResource type="scanner"/>
        |    </Task>
        |  </Tasks>
        |  <HumanResources>
        |    <Human id="HRS_1" name="Alice">
        |      <Handles type="printer"/>
        |      <Handles type="scanner"/>
        |    </Human>
        |  </HumanResources>
        |  <Products>
        |    <Product id="PRD_1" name="Product1">
        |      <Process tskref="TSK_1"/>
        |      <Process tskref="TSK_2"/>
        |    </Product>
        |  </Products>
        |  <Orders>
        |    <Order id="ORD_1" prdref="PRD_1" quantity="1"/>
        |  </Orders>
        |</Production>""".stripMargin

    val xml = XML.loadString(xmlString)
    val result = ScheduleMS03.create(xml)

    result match
      case Right(outputXml) =>
        val xmlString = outputXml.toString
        assert(xmlString.contains("Schedule"))
        assert(xmlString.contains("TSK_1"))
        assert(xmlString.contains("TSK_2"))
      case Left(error) =>
        fail(s"Expected successful XML processing but got: $error")

  test("create should fail with invalid XML data"):
    val xmlString =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<Production>
        |  <PhysicalResources>
        |    <Physical id="PRS_1" type="printer"/>
        |  </PhysicalResources>
        |  <Tasks>
        |    <Task id="TSK_1" time="0"> <!-- Invalid time -->
        |      <PhysicalResource type="printer"/>
        |    </Task>
        |  </Tasks>
        |  <HumanResources>
        |    <Human id="HRS_1" name="Alice">
        |      <Handles type="printer"/>
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
    val result = ScheduleMS03.create(xml)

    result match
      case Left(DomainError.InvalidTime("0")) =>
        succeed
      case other =>
        fail(s"Expected InvalidTime error but got: $other")