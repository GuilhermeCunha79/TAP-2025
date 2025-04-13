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
      @targetName("ProductTo")
      def to: String = id


  opaque type PhysicalId = String
  object PhysicalId:
    def from(id: String): Result[PhysicalId] =
      val pattern = "^PRS_.*$".r
      if pattern.matches(id) then Right(id)
      else Left(InvalidPhysicalId(id))

    extension (id: PhysicalId)
      @targetName("PhysicalTo")
      def to: String = id


  opaque type OrderId = String
  object OrderId:
    def from(id: String): Result[OrderId] =
      val pattern= "^ORD_.*$".r
      if pattern.matches(id) then Right(id)
      else Left(InvalidOrderId(id))

    extension (id: OrderId)
      @targetName("OrderTo")
      def to: String = id


  opaque type HumanId = String
  object HumanId:
    def from(id: String): Result[HumanId] =
      val pattern: Regex = "^HRS_.*$".r
      if pattern.matches(id) then Right(id)
      else Left(InvalidHumanId(id))

    extension (id: HumanId)
      @targetName("HumanTo")
      def to: String = id


  opaque type TaskId = String
  object TaskId:
    def from(id: String): Result[TaskId] =
      val pattern: Regex = "^TSK_.*$".r
      if pattern.matches(id) then Right(id)
      else Left(InvalidTaskId(id))

    extension (id: TaskId)
      @targetName("TaskTo")
      def to: String = id










  // To use Later
  //opaque type ID = String
  //object ID:
  //
  //  enum IDType(val pattern: Regex):
  //    case Physical extends IDType("^PRS_.*$".r)
  //    case Human    extends IDType("^HRS_.*$".r)
  //    case Task     extends IDType("^TSK_.*$".r)
  //    case Product  extends IDType("^PRD_.*$".r)
  //    case Order    extends IDType("^ORD_.*$".r)
  //
  //    // For IDREFs (if needed):
  //    case TaskRef     extends IDType("^TSK_.*$".r)
  //    case ProductRef  extends IDType("^PRD_.*$".r)
  //
  //  def from(id: String, idType: IDType): Result[ID] =
  //    if idType.pattern.matches(id) then Right(id)
  //    else Left(InvalidId(s"Invalid ID '$id' for type ${idType.toString}"))
  //
  //  // Extension methods for working with ID
  //  extension (id: ID)
  //    def value: String = id
