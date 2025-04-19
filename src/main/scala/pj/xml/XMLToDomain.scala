package pj.xml

import pj.domain.DomainError.*
import pj.domain.Result
import pj.domain.resources.*
import pj.domain.resources.Types.*

import scala.xml.{Elem, Node}

object XMLToDomain :
  def getProduct(tasks: List[Task])(xml: Node): Result[Product] =
    for
      id <- XML.fromAttribute(xml, "id")
      name <- XML.fromAttribute(xml, "name")
      productId <- ProductId.from(id)
      tasksList <- XML.traverse(xml \ "Process", { tasksNode =>
        XML.fromAttribute(tasksNode, "tskref").flatMap { taskRefString =>
          val isValidType = tasks.exists(_.id.to == taskRefString)
          if (isValidType) {
            TaskId.from(taskRefString)
          } else {
            Left(TaskDoesNotExist(taskRefString))
          }
        }
      })
    yield Product(productId, name, tasksList)

  def getOrder(products: List[Product])(xml: Node): Result[Order] =
    for
      rawOrderId <- XML.fromAttribute(xml, "id")
      rawProductId <- XML.fromAttribute(xml, "prdref")
      rawQuantity <- XML.fromAttribute(xml, "quantity")
      orderId <- OrderId.from(rawOrderId)
      quantity <- OrderQuantity.from(rawQuantity)
      productId <-
        val productExists = products.exists(_.id.to == rawProductId)
        if (productExists)
          ProductId.from(rawProductId)
        else
          Left(ProductDoesNotExist(rawProductId))
    yield Order(orderId, quantity, productId)

  def getPhysicalResource(xml: Node): Result[PhysicalResource] =
    for
      rawPhysicalResourceId <- XML.fromAttribute(xml, "id")
      physicalResourceType <- XML.fromAttribute(xml, "type")
      physicalResourceId <- PhysicalResourceId.from(rawPhysicalResourceId)
    yield PhysicalResource(physicalResourceId, physicalResourceType)

  def getTask(physicalResourceTypes: List[String])(xml: Node): Result[Task] =
    for
      rawTaskId <- XML.fromAttribute(xml, "id")
      rawTaskTime <- XML.fromAttribute(xml, "time")
      taskId <- TaskId.from(rawTaskId)
      taskTime <- TaskTime.from(rawTaskTime)
      taskPhysicalResources <- XML.traverse(xml \ "PhysicalResource", { physicalResourceNode =>
        XML.fromAttribute(physicalResourceNode, "type").flatMap { typeString =>
          val isValidType = physicalResourceTypes.contains(typeString)
          if (!isValidType) {
            Left(TaskUsesNonExistentPRT(typeString))
          } else {
            Right(typeString)
          }
        }
      })
    yield Task(taskId, taskTime, taskPhysicalResources)

  def getHumanResource(physicalResourceTypes: List[String])(xml: Node): Result[HumanResource] =
    for
      rawHumanResourceId <- XML.fromAttribute(xml, "id")
      humanResourceId <- HumanResourceId.from(rawHumanResourceId)
      humanResourceName <- XML.fromAttribute(xml, "name")
      humanResourcePhysicalResources <- XML.traverse(xml \ "Handles", { physicalResourceNode =>
        XML.fromAttribute(physicalResourceNode, "type").flatMap { typeString =>
          val isValidType = physicalResourceTypes.contains(typeString)
          if (!isValidType) {
            Left(PhysicalResourceTypeNotFound(s"$rawHumanResourceId,$typeString"))
          } else {
            Right(typeString)
          }
        }
      })
    yield HumanResource(humanResourceId, humanResourceName, humanResourcePhysicalResources)