package pj.domain.resources

import pj.domain.DomainError.*
import pj.domain.Result

import scala.util.matching.Regex

object Product:
  def from(id: String, name: String): Result[Product] =
    if ProductId.from(id).isRight
    then Right(Product(id, name))
    else Left(InvalidProductId(id))

object Order:
  def from(id: String, quantity: String, productId: String): Result[Order] =
    quantity.toIntOption match
      case None =>
        Left(InvalidQuantity(quantity))
      case Some(qi) if qi <= 0 =>
        Left(InvalidQuantity(quantity))
      case Some(qi) =>
        ProductId.from(productId) match
          case Left(_)  => Left(InvalidProductId(productId))
          case Right(pid) => Right(Order(id, qi, pid))

object Task:
  def from(id: String, time: String): Result[Task] =
    time.toIntOption match
      case None =>
        Left(InvalidTime(time))
      case Some(timeInt) if timeInt <= 0 =>
        Left(InvalidTime(time))
      case Some(timeInt) =>
        Right(Task(id, timeInt))

object PhysicalResourceType:
  def from(id: String, typeDescription: String) = ???

// ==================
// ID's
// ==================
opaque type ProductId = String

object ProductId:
  private val pattern: Regex = "^PRD_.*$".r

  def from(id: String): Result[ProductId] =
    if pattern.matches(id) then Right(id)
    else Left(InvalidProductId(id))

  extension (id: ProductId)
    def value: String = id


opaque type PhysicalId = String

object PhysicalId:
  private val pattern: Regex = "^PRS_.*$".r

  def from(id: String): Result[PhysicalId] =
    if pattern.matches(id) then Right(id)
    else Left(InvalidPhysicalId(id))

  extension (id: PhysicalId)
    def value: String = id


opaque type OrderId = String

object OrderId:
  private val pattern: Regex = "^ORD_.*$".r

  def from(id: String): Result[OrderId] =
    if pattern.matches(id) then Right(id)
    else Left(InvalidOrderId(id))

  extension (id: OrderId)
    def value: String = id


opaque type HumanId = String

object HumanId:
  private val pattern: Regex = "^HRS_.*$".r

  def from(id: String): Result[HumanId] =
    if pattern.matches(id) then Right(id)
    else Left(InvalidHumanId(id))

  extension (id: HumanId)
    def value: String = id


opaque type TaskId = String

object TaskId:
  private val pattern: Regex = "^TSK_.*$".r

  def from(id: String): Result[TaskId] =
    if pattern.matches(id) then Right(id)
    else Left(InvalidTaskId(id))

  extension (id: TaskId)
    def value: String = id




final case class Product(id: String, name: String)

final case class Order(id: String, quantity: Int, productId: ProductId)

final case class Task(id: String, time: Int)





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