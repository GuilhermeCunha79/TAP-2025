package pj.domain.resources

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.resources.Types.*
import pj.domain.DomainError.*
import pj.domain.*

class DomainTest extends AnyFunSuite:

  test("Product should be created successfully with valid ProductId and TaskIds"):
    val result =
      for
        productId <- ProductId.from("PRD_1")
        task1 <- TaskId.from("TSK_1")
        task2 <- TaskId.from("TSK_2")
        name <- ProductName.from("Widget")
      yield Product(productId, name, List(task1, task2))

    result match
      case Right(product) =>
        assert(product.id.to == "PRD_1")
        assert(product.name.to == "Widget")
        assert(product.tasksList.map(_.to) == List("TSK_1", "TSK_2"))
      case Left(err) => fail(s"Expected Product, but got error: $err")

  test("Order should be created successfully with valid values"):
    val result =
      for
        orderId <- OrderId.from("ORD_123")
        quantity <- OrderQuantity.from("5")
        productId <- ProductId.from("PRD_1")
      yield Order(orderId, quantity, productId)

    result match
      case Right(order) =>
        assert(order.id.to == "ORD_123")
        assert(order.quantity.to == 5)
        assert(order.productId.to == "PRD_1")
      case Left(err) => fail(s"Expected Order, but got error: $err")

  test("Task should be created successfully with valid values"):
    val result =
      for
        taskId <- TaskId.from("TSK_5")
        time <- TaskTime.from("30")
        type1 <- PhysicalResourceType.from("Drill")
        type2 <- PhysicalResourceType.from("Weld")
      yield Task(taskId, time, List(type1, type2))

    result match
      case Right(task) =>
        assert(task.id.to == "TSK_5")
        assert(task.time.to == 30)
        assert(task.physicalResourceTypes.map(_.to) == List("Drill", "Weld"))
      case Left(err) => fail(s"Expected Task, but got error: $err")

  test("HumanResource should be created successfully with valid HumanResourceId"):
    val result =
      for
        hrId <- HumanResourceId.from("HRS_42")
        name <- HumanResourceName.from("Alice")
        type1 <- PhysicalResourceType.from("Weld")
        type2 <- PhysicalResourceType.from("Assemble")
      yield HumanResource(hrId, name, List(type1, type2))

    result match
      case Right(hr) =>
        assert(hr.id.to == "HRS_42")
        assert(hr.name.to == "Alice")
        assert(hr.physicalResourceTypes.map(_.to) == List("Weld", "Assemble"))
      case Left(err) => fail(s"Expected HumanResource, but got error: $err")

  test("PhysicalResource should be created successfully with valid PhysicalResourceId"):
    val result =
      for
        prId <- PhysicalResourceId.from("PRS_99")
        prType <- PhysicalResourceType.from("Laser Cutter")
      yield PhysicalResource(prId, prType)

    result match
      case Right(pr) =>
        assert(pr.id.to == "PRS_99")
        assert(pr.physical_type.to == "Laser Cutter")
      case Left(err) => fail(s"Expected PhysicalResource, but got error: $err")

  test("Product creation should fail with invalid ProductId"):
    val result =
      for
        productId <- ProductId.from("INVALID")
        task1 <- TaskId.from("TSK_1")
        name <- ProductName.from("Faulty")
      yield Product(productId, name, List(task1))

    assert(result == Left(InvalidProductId("INVALID")))

  test("Product creation should fail if one TaskId is invalid"):
    val result =
      for
        productId <- ProductId.from("PRD_5")
        task1 <- TaskId.from("TSK_1")
        task2 <- TaskId.from("BAD_ID")
        name <- ProductName.from("MultiTask")
      yield Product(productId, name, List(task1, task2))

    assert(result == Left(InvalidTaskId("BAD_ID")))

  test("Order creation should fail with invalid quantity"):
    val result =
      for
        orderId <- OrderId.from("ORD_9")
        quantity <- OrderQuantity.from("0")
        productId <- ProductId.from("PRD_1")
      yield Order(orderId, quantity, productId)

    assert(result == Left(InvalidQuantity("0")))

  test("Order creation should fail with invalid OrderId"):
    val result =
      for
        orderId <- OrderId.from("WRONG_ID")
        quantity <- OrderQuantity.from("3")
        productId <- ProductId.from("PRD_1")
      yield Order(orderId, quantity, productId)

    assert(result == Left(InvalidOrderId("WRONG_ID")))

  test("Task creation should fail with invalid time"):
    val result =
      for
        taskId <- TaskId.from("TSK_9")
        time <- TaskTime.from("-1")
        type1 <- PhysicalResourceType.from("Cut")
        type2 <- PhysicalResourceType.from("Weld")
      yield Task(taskId, time, List(type1, type2))

    assert(result == Left(InvalidTime("-1")))

  test("Task creation should fail with invalid TaskId"):
    val result =
      for
        taskId <- TaskId.from("XXX")
        time <- TaskTime.from("10")
        type1 <- PhysicalResourceType.from("Assemble")
      yield Task(taskId, time, List(type1))

    assert(result == Left(InvalidTaskId("XXX")))

  test("HumanResource creation should fail with invalid HumanResourceId"):
    val result =
      for
        hrId <- HumanResourceId.from("ID_999")
        name <- HumanResourceName.from("Bob")
        type1 <- PhysicalResourceType.from("Drill")
      yield HumanResource(hrId, name, List(type1))

    assert(result == Left(InvalidHumanId("ID_999")))

  test("PhysicalResource creation should fail with invalid PhysicalResourceId"):
    val result =
      for
        prId <- PhysicalResourceId.from("BADPRS")
        type1 <- PhysicalResourceType.from("Faulty Tool")
      yield PhysicalResource(prId, type1)

    assert(result == Left(InvalidPhysicalId("BADPRS")))

  test("TaskSchedule creation with valid opaque types should succeed"):
    val result =
      for
        orderId <- OrderId.from("ORD_100")
        taskId <- TaskId.from("TSK_10")
        pr1 <- PhysicalResourceId.from("PRS_1")
        pr2 <- PhysicalResourceId.from("PRS_2")
        name1 <- HumanResourceId.from("HRS_1")
        name2 <- HumanResourceId.from("HRS_2")
        productNumber <- ProductNumber.from(1)
        start <- TaskScheduleTime.from(0)
        end <- TaskScheduleTime.from(10)
      yield TaskSchedule(
        orderId = orderId,
        productNumber = productNumber,
        taskId = taskId,
        start = start,
        end = end,
        physicalResourceIds = List(pr1, pr2),
        humanResourceIds = List(name1, name2)
      )

    result match
      case Right(schedule) =>
        assert(
          schedule.orderId.to == "ORD_100" &&
            schedule.productNumber.to == 1 &&
            schedule.taskId.to == "TSK_10" &&
            schedule.start.to == 0 &&
            schedule.end.to == 10 &&
            schedule.physicalResourceIds.map(_.to) == List("PRS_1", "PRS_2") &&
            schedule.humanResourceIds.map(_.to) == List("HRS_1", "HRS_2")
        )
      case Left(err) => fail(s"Expected TaskSchedule, but got error: $err")

  test("TaskSchedule creation should fail with invalid OrderId"):
    val result =
      for
        orderId <- OrderId.from("INVALID")
        taskId <- TaskId.from("TSK_10")
        pr <- PhysicalResourceId.from("PRS_1")
        hr <- HumanResourceId.from("HRS_1")
        productNumber <- ProductNumber.from(1)
        start <- TaskScheduleTime.from(0)
        end <- TaskScheduleTime.from(10)
      yield TaskSchedule(orderId, productNumber, taskId, start, end, List(pr), List(hr))

    assert(result == Left(InvalidOrderId("INVALID")))

  test("TaskSchedule creation should fail with invalid TaskId"):
    val result =
      for
        orderId <- OrderId.from("ORD_1")
        taskId <- TaskId.from("WRONG")
        pr <- PhysicalResourceId.from("PRS_1")
        hr <- HumanResourceId.from("HRS_1")
        productNumber <- ProductNumber.from(1)
        start <- TaskScheduleTime.from(0)
        end <- TaskScheduleTime.from(10)
      yield TaskSchedule(orderId, productNumber, taskId, start, end, List(pr), List(hr))

    assert(result == Left(InvalidTaskId("WRONG")))

  test("TaskSchedule creation should fail if one PhysicalResourceId is invalid"):
    val valid = for
      pr1 <- PhysicalResourceId.from("PRS_1")
      pr2 <- PhysicalResourceId.from("INVALID_ID")
    yield (pr1, pr2)

    assert(valid == Left(InvalidPhysicalId("INVALID_ID")))

  test("TaskSchedule with empty humanResourceNames should still succeed"):
    val result =
      for
        orderId <- OrderId.from("ORD_2")
        taskId <- TaskId.from("TSK_2")
        pr <- PhysicalResourceId.from("PRS_10")
        productNumber <- ProductNumber.from(5)
        start <- TaskScheduleTime.from(100)
        end <- TaskScheduleTime.from(200)
      yield TaskSchedule(orderId, productNumber, taskId, start, end, List(pr), List.empty)

    result match
      case Right(schedule) =>
        assert(
          schedule.orderId.to == "ORD_2" &&
            schedule.taskId.to == "TSK_2" &&
            schedule.humanResourceIds.isEmpty
        )
      case Left(err) => fail(s"Expected TaskSchedule, but got error: $err")

  test("TaskInfo should be created successfully with valid data"):
    val result = for {
      orderId <- OrderId.from("ORD_1")
      productNumber <- ProductNumber.from(1)
      taskId <- TaskId.from("TSK_1")
      taskTime <- TaskTime.from("15")
      resType <- PhysicalResourceType.from("Paint")
      earliestStart <- EarliestStartTime.from(0)
      index <- ProductTaskIndex.from(0)
    } yield
      val task = Task(taskId, taskTime, List(resType))
      TaskInfo(orderId, productNumber, taskId, task, earliestStart, index)
  
    result match
      case Right(taskInfo) =>
        assert(taskInfo.orderId.to == "ORD_1")
        assert(taskInfo.productNumber.to == 1)
        assert(taskInfo.taskId.to == "TSK_1")
        assert(taskInfo.earliestStart.to == 0)
        assert(taskInfo.productTaskIndex.to == 0)
      case Left(err) => fail(s"Expected TaskInfo, but got error: $err")

  test("SchedulingState should be created with initial data and empty progress"):
    val result = for {
      orderId <- OrderId.from("ORD_1")
      productNumber <- ProductNumber.from(1)
      taskId <- TaskId.from("TSK_1")
      taskTime <- TaskTime.from("20")
      resType <- PhysicalResourceType.from("Assemble")
      earliestStart <- EarliestStartTime.from(5)
      index <- ProductTaskIndex.from(0)
    } yield
      val task = Task(taskId, taskTime, List(resType))
      val taskInfo = TaskInfo(orderId, productNumber, taskId, task, earliestStart, index)
      val availability = Map("PHYS_PRS_1" -> 0, "HUMAN_HRS_1" -> 0)

      SchedulingState(
        readyTasks = List(taskInfo),
        resourceAvailability = availability,
        schedules = Nil,
        productProgress = Map.empty
      )

    result match
      case Right(state) =>
        assert(state.readyTasks.nonEmpty)
        assert(state.resourceAvailability.sizeIs == 2)
        assert(state.schedules.isEmpty)
        assert(state.productProgress.isEmpty)
      case Left(err) => fail(s"Expected SchedulingState, but got error: $err")

