package pj.xml

import pj.domain.DomainError.*
import pj.domain.Result
import pj.domain.resources.*
import pj.domain.resources.Types.*

import scala.xml.{Elem, Node}

object XMLToDomain :
//  def getProduct(tasks: List[Task])(xml: Node): Result[Product] =
  def getProduct(xml: Node): Result[Product] =
    for
      id <- XML.fromAttribute(xml, "id")
      name <- XML.fromAttribute(xml, "name")
      productId <- ProductId.from(id)
    yield Product(productId, name)

  def getOrder(xml: Node): Result[Order] =
    for
      rawOrderId <- XML.fromAttribute(xml, "id")
      rawProductId <- XML.fromAttribute(xml, "prdref")
      rawQuantity <- XML.fromAttribute(xml, "quantity")
      orderId <- OrderId.from(rawOrderId)
      productId <- ProductId.from(rawProductId)
      quantity <- OrderQuantity.from(rawQuantity)
    yield Order(orderId, quantity, productId)

  def getPhysicalResource(xml: Node): Result[PhysicalResource] =
    for
      rawPhysicalResourceId <- XML.fromAttribute(xml, "id")
      rawPhysicalResourceType <- XML.fromAttribute(xml, "type")
      physicalResourceId <- PhysicalResourceId.from(rawPhysicalResourceId)
      physicalResourceType <- PhysicalResourceType.from(rawPhysicalResourceType)
    yield PhysicalResource(physicalResourceId, physicalResourceType)
  
  def getTask(physicalResourceTypes: List[PhysicalResourceType])(xml: Node): Result[Task] =
    for
      rawTaskId <- XML.fromAttribute(xml, "id")
      rawTaskTime <- XML.fromAttribute(xml, "time")
      taskId <- TaskId.from(rawTaskId)
      taskTime <- TaskTime.from(rawTaskTime)
      taskPhysicalResources <- XML.traverse(xml \ "PhysicalResource", { physicalResourceNode =>
        XML.fromAttribute(physicalResourceNode, "type").flatMap { typeString =>
          val isValidType = physicalResourceTypes.exists(_.to == typeString)
          if (isValidType) {
            PhysicalResourceType.from(typeString)
          } else {
            Left(PhysicalResourceTypeNotFound(s"Task '$rawTaskId' references unknown PhysicalResource type: '$typeString'"))
          }
        }
      })
    yield Task(taskId, taskTime, taskPhysicalResources)