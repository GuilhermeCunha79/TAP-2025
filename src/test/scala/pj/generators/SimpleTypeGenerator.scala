package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.Types.{HumanResourceId, HumanResourceName, OrderId, OrderQuantity, PhysicalResourceId, PhysicalResourceType, ProductId, ProductName, ProductNumber, TaskId, TaskScheduleTime, TaskTime}

object SimpleTypeGenerator extends Properties("SimpleTypes"):

  val suffixGen: Gen[String] = for {
    firstChar <- Gen.alphaNumChar
    rest <- Gen.listOf(Gen.alphaNumChar)
  } yield (firstChar :: rest).mkString

  //Product
  def ProductIdGenerator: Gen[ProductId] =
    suffixGen.flatMap(id => ProductId.from(s"PRD_$id") match
      case Right(pId) => Gen.const(pId)
      case Left(_) => Gen.fail
    )

  def ProductNameGenerator: Gen[ProductName] =
    suffixGen.flatMap(prdName => ProductName.from(prdName) match
      case Right(prodName) => Gen.const(prodName)
      case Left(_) => Gen.fail
    )

  def ProductNumberGenerator: Gen[ProductNumber] =
    Gen.posNum[Int].flatMap(prodNumber => ProductNumber.from(prodNumber) match
      case Right(prdNumber) => Gen.const(prdNumber)
      case Left(_) => Gen.fail
    )

  //Order
  def OrderIdGenerator: Gen[OrderId] =
    suffixGen.flatMap(id => OrderId.from(s"ORD_$id") match
      case Right(ordId) => Gen.const(ordId)
      case Left(_) => Gen.fail
    )

  def OrderQuantityGenerator: Gen[OrderQuantity] =
    Gen.posNum[Int].map(_.toString).flatMap(orderQtd => OrderQuantity.from(orderQtd) match
      case Right(qty) => Gen.const(qty)
      case Left(_) => Gen.fail
    )

  //Task
  def TaskIdGenerator: Gen[TaskId] =
    suffixGen.flatMap(id => TaskId.from(s"TSK_$id") match
      case Right(tId) => Gen.const(tId)
      case Left(_) => Gen.fail
    )

  def TaskTimeGenerator: Gen[TaskTime] =
    Gen.posNum[Int].map(_.toString).flatMap(tskTime => TaskTime.from(tskTime) match
      case Right(time) => Gen.const(time)
      case Left(_) => Gen.fail
    )

  def TaskScheduleTimeGenerator: Gen[TaskScheduleTime] =
    Gen.posNum[Int].flatMap(tskSchedTime => TaskScheduleTime.from(tskSchedTime) match
      case Right(tskSchTime) => Gen.const(tskSchTime)
      case Left(_) => Gen.fail
    )

  //Physical Resources
  def PhysicalResourceIdGenerator: Gen[PhysicalResourceId] =
    suffixGen.flatMap(id => PhysicalResourceId.from(s"PRS_$id") match
      case Right(prId) => Gen.const(prId)
      case Left(_) => Gen.fail
    )

  def PhysicalResourceTypeGenerator: Gen[PhysicalResourceType] =
    suffixGen.flatMap(name => PhysicalResourceType.from(name) match
      case Right(pType) => Gen.const(pType)
      case Left(_) => Gen.fail
    )

  //Human Resources
  def HumanResourceIdGenerator: Gen[HumanResourceId] =
    suffixGen.flatMap(id => HumanResourceId.from(s"HRS_$id") match
      case Right(hId) => Gen.const(hId)
      case Left(_) => Gen.fail
    )

  def HumanResourceNameGenerator: Gen[HumanResourceName] =
    suffixGen.flatMap(name => HumanResourceName.from(name) match
      case Right(hName) => Gen.const(hName)
      case Left(_) => Gen.fail
    )