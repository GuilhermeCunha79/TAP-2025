package pj.domain.resources

import pj.domain.DomainError.*
import pj.domain.Result

import scala.annotation.targetName
import scala.util.matching.Regex

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
      val pattern= "^ORD_.*$".r
      if pattern.matches(id) then Right(id)
      else Left(InvalidOrderId(id))

    extension (id: OrderId)
      @targetName("OrderIdTo")
      def to: String = id


  opaque type HumanResourceId = String
  object HumanResourceId:
    def from(id: String): Result[HumanResourceId] =
      val pattern: Regex = "^HRS_.*$".r
      if pattern.matches(id) then Right(id)
      else Left(InvalidHumanId(id))

    extension (id: HumanResourceId)
      @targetName("HumanResourceIdTo")
      def to: String = id


  opaque type TaskId = String
  object TaskId:
    def from(id: String): Result[TaskId] =
      val pattern: Regex = "^TSK_.*$".r
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


//  opaque type PositiveInteger = Int
//  object PositiveInteger:
//    def from(positiveInteger: String): Result[PositiveInteger] =
//      positiveInteger.toIntOption match
//        case None =>
//          Left(InvalidTime(positiveInteger))
//        case Some(positiveInt) if positiveInt <= 0 =>
//          Left(InvalidTime(positiveInteger))
//        case Some(positiveInt) =>
//          Right(positiveInt)
//
//    extension (positiveInt: PositiveInteger)
//      @targetName("PositiveIntegerTo")
//      def to: Int = positiveInt
