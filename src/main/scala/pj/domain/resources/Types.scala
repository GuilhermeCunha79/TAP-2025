package pj.domain.resources

import pj.domain.DomainError.*
import pj.domain.Result

import scala.annotation.targetName

object Types :

  opaque type ProductId = String
  object ProductId:
    def from(id: String): Result[ProductId] =
      val pattern = "^PRD_.*$".r
      if pattern.matches(id) then Right(id)
      else Left(InvalidProductId(id))

    extension (id: ProductId)
      @targetName("ProductIdTo")
      def to: String = id


  opaque type PhysicalResourceId = String
  object PhysicalResourceId:
    def from(id: String): Result[PhysicalResourceId] =
      val pattern = "^PRS_.*$".r
      if pattern.matches(id) then Right(id)
      else Left(InvalidPhysicalId(id))

    extension (id: PhysicalResourceId)
      @targetName("PhysicalResourceIdTo")
      def to: String = id


  opaque type OrderId = String
  object OrderId:
    def from(id: String): Result[OrderId] =
      val pattern = "^ORD_.*$".r
      if pattern.matches(id) then Right(id)
      else Left(InvalidOrderId(id))

    extension (id: OrderId)
      @targetName("OrderIdTo")
      def to: String = id


  opaque type HumanResourceId = String
  object HumanResourceId:
    def from(id: String): Result[HumanResourceId] =
      val pattern = "^HRS_.*$".r
      if pattern.matches(id) then Right(id)
      else Left(InvalidHumanId(id))

    extension (id: HumanResourceId)
      @targetName("HumanResourceIdTo")
      def to: String = id


  opaque type TaskId = String
  object TaskId:
    def from(id: String): Result[TaskId] =
      val pattern = "^TSK_.*$".r
      if pattern.matches(id) then Right(id)
      else Left(InvalidTaskId(id))

    extension (id: TaskId)
      @targetName("TaskIdTo")
      def to: String = id

  
  opaque type OrderQuantity = Int
  object OrderQuantity:
    def from(quantity: String): Result[OrderQuantity] =
      quantity.toIntOption match
        case None =>
          Left(InvalidQuantity(quantity))
        case Some(quantityInt) if quantityInt <= 0 =>
          Left(InvalidQuantity(quantity))
        case Some(quantityInt) =>
          Right(quantityInt)

    extension (quantityInt: OrderQuantity)
      @targetName("OrderQuantityTo")
      def to: Int = quantityInt


  opaque type TaskTime = Int
  object TaskTime:
    def from(time: String): Result[TaskTime] =
      time.toIntOption match
        case None =>
          Left(InvalidTime(time))
        case Some(timeInt) if timeInt <= 0 =>
          Left(InvalidTime(time))
        case Some(timeInt) =>
          Right(timeInt)

    extension (timeInt: TaskTime)
      @targetName("TaskTimeTo")
      def to: Int = timeInt


  opaque type ProductNumber = Int
  object ProductNumber:
    def from(productNumber: Int): Result[ProductNumber] =
      if productNumber <= 0
      then Left(InvalidProductNumber(productNumber.toString))
      else Right(productNumber)

    extension (productNumberInt: ProductNumber)
      @targetName("ProductNumberTo")
      def to: Int = productNumberInt


  opaque type TaskScheduleTime = Int
  object TaskScheduleTime:
    def from(taskScheduleTime: Int): Result[TaskScheduleTime] =
      if taskScheduleTime < 0
      then Left(InvalidTaskScheduleTime(taskScheduleTime.toString))
      else Right(taskScheduleTime)

    extension (taskScheduleTimeInt: TaskScheduleTime)
      @targetName("TaskScheduleTimeTo")
      def to: Int = taskScheduleTimeInt
