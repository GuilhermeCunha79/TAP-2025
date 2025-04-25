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
      yield Product(productId, "Widget", List(task1, task2))

    result match
      case Right(product) =>
        assert(product.id.to == "PRD_1")
        assert(product.name == "Widget")
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
      yield Task(taskId, time, List("Drill", "Weld"))

    result match
      case Right(task) =>
        assert(task.id.to == "TSK_5")
        assert(task.time.to == 30)
        assert(task.physicalResourceTypes == List("Drill", "Weld"))
      case Left(err) => fail(s"Expected Task, but got error: $err")

  test("HumanResource should be created successfully with valid HumanResourceId"):
    val result =
      for
        hrId <- HumanResourceId.from("HRS_42")
      yield HumanResource(hrId, "Alice", List("Weld", "Assemble"))

    result match
      case Right(hr) =>
        assert(hr.id.to == "HRS_42")
        assert(hr.name == "Alice")
        assert(hr.physicalResourceTypes == List("Weld", "Assemble"))
      case Left(err) => fail(s"Expected HumanResource, but got error: $err")

  test("PhysicalResource should be created successfully with valid PhysicalResourceId"):
    val result =
      for
        prId <- PhysicalResourceId.from("PRS_99")
      yield PhysicalResource(prId, "Laser Cutter")

    result match
      case Right(pr) =>
        assert(pr.id.to == "PRS_99")
        assert(pr.name == "Laser Cutter")
      case Left(err) => fail(s"Expected PhysicalResource, but got error: $err")

  test("Product creation should fail with invalid ProductId"):
    val result =
      for
        productId <- ProductId.from("INVALID")
        task1 <- TaskId.from("TSK_1")
      yield Product(productId, "Faulty", List(task1))

    assert(result == Left(InvalidProductId("INVALID")))

  test("Product creation should fail if one TaskId is invalid"):
    val result =
      for
        productId <- ProductId.from("PRD_5")
        task1 <- TaskId.from("TSK_1")
        task2 <- TaskId.from("BAD_ID")
      yield Product(productId, "MultiTask", List(task1, task2))

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
      yield Task(taskId, time, List("Cut", "Weld"))

    assert(result == Left(InvalidTime("-1")))

  test("Task creation should fail with invalid TaskId"):
    val result =
      for
        taskId <- TaskId.from("XXX")
        time <- TaskTime.from("10")
      yield Task(taskId, time, List("Assemble"))

    assert(result == Left(InvalidTaskId("XXX")))

  test("HumanResource creation should fail with invalid HumanResourceId"):
    val result =
      for
        hrId <- HumanResourceId.from("ID_999")
      yield HumanResource(hrId, "Bob", List("Drill"))

    assert(result == Left(InvalidHumanId("ID_999")))

  test("PhysicalResource creation should fail with invalid PhysicalResourceId"):
    val result =
      for
        prId <- PhysicalResourceId.from("BADPRS")
      yield PhysicalResource(prId, "Faulty Tool")

    assert(result == Left(InvalidPhysicalId("BADPRS")))

  test("TaskSchedule creation with valid opaque types should succeed"):
    val result =
      for
        orderId <- OrderId.from("ORD_100")
        taskId <- TaskId.from("TSK_10")
        pr1 <- PhysicalResourceId.from("PRS_1")
        pr2 <- PhysicalResourceId.from("PRS_2")
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
        humanResourceNames = List("Alice", "Bob")
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
            schedule.humanResourceNames == List("Alice", "Bob")
        )
      case Left(err) => fail(s"Expected TaskSchedule, but got error: $err")

  test("TaskSchedule creation should fail with invalid OrderId"):
    val result =
      for
        orderId <- OrderId.from("INVALID")
        taskId <- TaskId.from("TSK_10")
        pr <- PhysicalResourceId.from("PRS_1")
        productNumber <- ProductNumber.from(1)
        start <- TaskScheduleTime.from(0)
        end <- TaskScheduleTime.from(10)
      yield TaskSchedule(orderId, productNumber, taskId, start, end, List(pr), List("Alice"))

    assert(result == Left(InvalidOrderId("INVALID")))

  test("TaskSchedule creation should fail with invalid TaskId"):
    val result =
      for
        orderId <- OrderId.from("ORD_1")
        taskId <- TaskId.from("WRONG")
        pr <- PhysicalResourceId.from("PRS_1")
        productNumber <- ProductNumber.from(1)
        start <- TaskScheduleTime.from(0)
        end <- TaskScheduleTime.from(10)
      yield TaskSchedule(orderId, productNumber, taskId, start, end, List(pr), List("Bob"))

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
            schedule.humanResourceNames.isEmpty
        )
      case Left(err) => fail(s"Expected TaskSchedule, but got error: $err")
