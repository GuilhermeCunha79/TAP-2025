package pj.domain.resources

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError.*
import pj.domain.resources.Types.*
import pj.domain.{DomainError, Result}

class TypesTest extends AnyFunSuite:

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

  test("ProductNumber.from with valid input returns Right"):
    val result = ProductNumber.from(5)
    assert(result.fold(_ => false, pn => pn.to == 5))

  test("ProductNumber.from with zero returns Left"):
    val result = ProductNumber.from(0)
    assert(result == Left(InvalidProductNumber("0")))

  test("ProductNumber.from with negative number returns Left"):
    val result = ProductNumber.from(-3)
    assert(result == Left(InvalidProductNumber("-3")))

  test("TaskScheduleTime.from with valid input returns Right"):
    val result = TaskScheduleTime.from(10)
    assert(result.fold(_ => false, time => time.to == 10))

  test("TaskScheduleTime.from with zero returns Right"):
    val result = TaskScheduleTime.from(0)
    assert(result.fold(_ => false, time => time.to == 0))

  test("TaskScheduleTime.from with negative number returns Left"):
    val result = TaskScheduleTime.from(-1)
    assert(result == Left(InvalidTaskScheduleTime("-1")))

  test("ProductName.from with non-empty string returns Right"):
    val result = ProductName.from("Widget")
    assert(result.fold(_ => false, name => name.to == "Widget"))

  test("ProductName.from with empty string returns Left"):
    val result = ProductName.from("")
    assert(result == Left(EmptyProductName("")))

  test("ProductName.equal returns true for equal names"):
    val result =
      for
        name1 <- ProductName.from("Gadget")
        name2 <- ProductName.from("Gadget")
      yield name1.equal(name2)

    assert(result.contains(true))

  test("ProductName.equal returns false for different names"):
    val result =
      for
        name1 <- ProductName.from("Gadget")
        name2 <- ProductName.from("Widget")
      yield name1.equal(name2)

    assert(result.contains(false))

  test("HumanResourceName.from with valid name returns Right"):
    val result = HumanResourceName.from("Alice")
    assert(result.fold(_ => false, name => name.to == "Alice"))

  test("HumanResourceName.from with empty string returns Left"):
    val result = HumanResourceName.from("")
    assert(result == Left(EmptyHumanResourceName("")))

  test("HumanResourceName.equal returns true for equal names"):
    val result =
      for
        name1 <- HumanResourceName.from("Bob")
        name2 <- HumanResourceName.from("Bob")
      yield name1.equal(name2)

    assert(result.contains(true))

  test("HumanResourceName.equal returns false for different names"):
    val result =
      for
        name1 <- HumanResourceName.from("Alice")
        name2 <- HumanResourceName.from("Bob")
      yield name1.equal(name2)

    assert(result.contains(false))

  test("PhysicalResourceType.from with valid type returns Right"):
    val result = PhysicalResourceType.from("Drill")
    assert(result.fold(_ => false, t => t.to == "Drill"))

  test("PhysicalResourceType.from with empty string returns Left"):
    val result = PhysicalResourceType.from("")
    assert(result == Left(EmptyPhysicalResourceType("")))

  test("PhysicalResourceType.equal returns true for equal types"):
    val result =
      for
        t1 <- PhysicalResourceType.from("Cutter")
        t2 <- PhysicalResourceType.from("Cutter")
      yield t1.equal(t2)

    assert(result.contains(true))

  test("PhysicalResourceType.equal returns false for different types"):
    val result =
      for
        t1 <- PhysicalResourceType.from("Cutter")
        t2 <- PhysicalResourceType.from("Welder")
      yield t1.equal(t2)

    assert(result.contains(false))

  test("EarliestStartTime.from with negative number returns Left"):
    val result = EarliestStartTime.from(-1)
    assert(result == Left(InvalidEarliestStartTime("-1")))

  test("EarliestStartTime.from with zero returns Right"):
    val result = EarliestStartTime.from(0)
    assert(result.fold(_ => false, _.to == 0))

  test("EarliestStartTime.from with positive number returns Right"):
    val result = EarliestStartTime.from(5)
    assert(result.fold(_ => false, _.to == 5))

  test("ProductTaskIndex.from with negative number returns Left"):
    val result = ProductTaskIndex.from(-1)
    assert(result == Left(InvalidProductTaskIndex("-1")))

  test("ProductTaskIndex.from with zero returns Right"):
    val result = ProductTaskIndex.from(0)
    assert(result.fold(_ => false, _.to == 0))

  test("ProductTaskIndex.from with positive number returns Right"):
    val result = ProductTaskIndex.from(3)
    assert(result.fold(_ => false, _.to == 3))