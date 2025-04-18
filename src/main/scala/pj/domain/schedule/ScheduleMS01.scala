package pj.domain.schedule

import pj.domain.*
import pj.domain.resources.*
import pj.domain.resources.Types.*
import pj.xml.{XML, XMLToDomain}

import scala.xml.Elem

object ScheduleMS01 extends Schedule:

  def ScheduleDataRetriever(xml: Elem): Result[(List[PhysicalResource], List[PhysicalResourceType], List[Task], List[HumanResource], List[Product], List[Order])] =
    for
      physicalNode <- XML.fromNode(xml, "PhysicalResources")
      physicalResources <- XML.traverse(physicalNode \ "Physical", XMLToDomain.getPhysicalResource)
      physicalTypes = physicalResources.map(_.name).distinct

      tasksNode <- XML.fromNode(xml, "Tasks")
      tasks <- XML.traverse(tasksNode \ "Task", XMLToDomain.getTask(physicalTypes))

      humanNode <- XML.fromNode(xml, "HumanResources")
      humanResources <- XML.traverse(humanNode \ "Human", XMLToDomain.getHumanResource(physicalTypes))

      productsNode <- XML.fromNode(xml, "Products")
      products <- XML.traverse(productsNode \ "Product", XMLToDomain.getProduct(tasks))

      ordersNode <- XML.fromNode(xml, "Orders")
      orders <- XML.traverse(ordersNode \ "Order", XMLToDomain.getOrder(products))
    yield
      (physicalResources, physicalTypes, tasks, humanResources, products, orders)

  def FullLogic(physicalResources: List[PhysicalResource],
                 physicalTypes: List[PhysicalResourceType],
                 tasks: List[Task],
                 humanResources: List[HumanResource],
                 products: List[Product],
                 orders: List[Order]
               ): Elem =

    val accumulatedSchedules = orders.flatMap { order =>
      val order_orderId = order.id
      val order_productId = order.productId
      val order_quantity = order.quantity

      (1 to order_quantity.to).foldLeft(List.empty[(Int, List[PhysicalResourceId], List[String], Int)]) { (accumulatedSchedules, productNumber) =>
        val productOpt = products.find(_.id == order_productId)

        productOpt match
          case Some(p) =>
            val product_tasks = p.tasksList

            product_tasks.foldLeft(accumulatedSchedules) { (acc, taskId) =>
              val taskOpt = tasks.find(_.id == taskId)

              taskOpt match
                case Some(t) =>
                  val task_taskId = t.id
                  val task_taskTime = t.time
                  val task_physicalResourceTypes = t.physicalResources

                  val (updatedPhysicalResourceIds, updatedHumanResourceNames) =
                    task_physicalResourceTypes.foldLeft((List.empty[PhysicalResourceId], List.empty[String])):
                      case ((prList, hrList), p_physicalResource) =>
                        val updatedPrList = physicalResources.find(_.name == p_physicalResource).map(pr => pr.id :: prList).getOrElse(prList)
                        val updatedHrList = humanResources.filter(_.physicalResources.contains(p_physicalResource)).map(_.name) ++ hrList
                        (updatedPrList, updatedHrList)
                  
                  val start = acc.headOption.map(_._1).getOrElse(0)
                  val end = start + task_taskTime.to
                  val updatedPhysicalResourceIdsReverse = updatedPhysicalResourceIds.reverse
                  val updatedHumanResourceNamesReverse = updatedHumanResourceNames.reverse

                  println(
                    s"Task Scheduled: Order ID: $order_orderId, Product Number $productNumber, Task ID: $task_taskId, Start: $start, End: $end, " +
                      s"Physical Resources: $updatedPhysicalResourceIdsReverse, Human Resources: $updatedHumanResourceNamesReverse"
                  )

                  (end, updatedPhysicalResourceIdsReverse, updatedHumanResourceNamesReverse, end) :: acc

                case None => acc
            }

          case None => accumulatedSchedules
      }
    }

    val reversedSchedules = accumulatedSchedules.reverse

    <schedules></schedules>


  // TODO: Create the code to implement a functional domain model for schedule creation
  //       Use the xml.XML code to handle the xml elements
  //       Refer to https://github.com/scala/scala-xml/wiki/XML-Processing for xml creation
  def create(xml: Elem): Result[Elem] =
    val domain = for
      value <- ScheduleDataRetriever(xml)
    yield value

    domain match
      case Right(value) => Right(FullLogic(value._1, value._2, value._3, value._4, value._5, value._6))
      case Left(error) => Left(error)