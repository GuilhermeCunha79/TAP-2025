package pj.domain.schedule

import pj.domain.*
import pj.domain.resources.*
import pj.domain.resources.Types.*
import pj.xml.{XML, XMLToDomain}

import scala.xml.Elem

object ScheduleMS01 extends Schedule {

  private def scheduleDataRetriever(xml: Elem): Result[(
    List[PhysicalResource],
    List[PhysicalResourceType],
    List[Task],
    List[HumanResource],
    List[Product],
    List[Order]
  )] =
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

  private def allocatePhysicalResources(
    taskId: TaskId,
    requiredTypes: List[PhysicalResourceType],
    availableResources: List[PhysicalResource]
  ): Result[List[PhysicalResourceId]] =
    requiredTypes.foldLeft[Result[(List[PhysicalResourceId], Set[PhysicalResourceId])]](Right((Nil, Set.empty))) {
      case (accResult, requiredType) => accResult.flatMap { case (assignedIds, usedIds) =>
        availableResources
          .find(resource => resource.name.equal(requiredType) && !usedIds.contains(resource.id))
          .map(_.id)
          .toRight(DomainError.ResourceUnavailable(taskId.to, requiredType.to))
          .map(id => (id :: assignedIds, usedIds + id))
      }
    }.map(_._1.reverse)

  private def allocateHumanResources(
    taskId: TaskId,
    requiredTypes: List[PhysicalResourceType],
    availableHumans: List[HumanResource]
  ): Result[List[String]] =
    requiredTypes.foldLeft[Result[(List[String], Set[String])]](Right((Nil, Set.empty))) {
      case (accResult, requiredType) => accResult.flatMap { case (assignedNames, usedNames) =>
        availableHumans
          .find(hr => hr.physicalResources.contains(requiredType) && !usedNames.contains(hr.name))
          .toRight(DomainError.ResourceUnavailable(taskId.to, requiredType.to))
          .map(hr => (hr.name :: assignedNames, usedNames + hr.name))
      }
    }.map(_._1.reverse)

  private def generateSchedule(
      physicalResources: List[PhysicalResource],
      physicalTypes: List[PhysicalResourceType],
      allTasks: List[Task],
      humanResources: List[HumanResource],
      allProducts: List[Product],
      allOrders: List[Order]
    ): Result[Elem] =

    type Accumulator = (List[TaskSchedule], Int)
    val initialAccumulator: Result[Accumulator] = Right((Nil, 0))

    val scheduleResult = allOrders.foldLeft[Result[Accumulator]](initialAccumulator) { (accumulatorForOrders, currentOrder) =>
      accumulatorForOrders.flatMap { case (scheduledTasksSoFar, globalTimeSoFar) =>
        val matchedProduct = allProducts.find(_.id == currentOrder.productId)
          .toRight(DomainError.ProductDoesNotExist(currentOrder.productId.to))

        (1 to currentOrder.quantity.to).foldLeft[Result[Accumulator]](Right((scheduledTasksSoFar, globalTimeSoFar))):
          case (accumulatorForProducts, productInstanceNumber) => accumulatorForProducts.flatMap:
            case (tasksForProduct, timeAtStartOfProduct) => matchedProduct.flatMap { productDef =>
              productDef.tasksList.foldLeft[Result[Accumulator]](Right((tasksForProduct, timeAtStartOfProduct))):
                case (accumulatorForTasks, taskId) => accumulatorForTasks.flatMap:
                  case (taskList, taskStartTime) =>
                    allTasks.find(_.id == taskId)
                      .toRight(DomainError.TaskDoesNotExist(taskId.to))
                      .flatMap { task =>
                        for {
                          allocatedPhysicalResources <- allocatePhysicalResources(task.id, task.physicalResources, physicalResources)
                          allocatedHumanResources <- allocateHumanResources(task.id, task.physicalResources, humanResources)
                        } yield
                          val taskEndTime = taskStartTime + task.time.to
                          val taskSchedule = TaskSchedule(
                            currentOrder.id,
                            productInstanceNumber,
                            task.id,
                            taskStartTime,
                            taskEndTime,
                            allocatedPhysicalResources,
                            allocatedHumanResources
                          )
                          (taskSchedule :: taskList, taskEndTime)
                      }
              }
      }
    }

    scheduleResult.map { case (allFinalSchedules, _) =>
      toXml(allFinalSchedules)
    }

  // Function to generate the XML
  private def toXml(schedules: List[TaskSchedule]): Elem =
    <Schedule xmlns="http://www.dei.isep.ipp.pt/tap-2025"
              xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
              xsi:schemaLocation="http://www.dei.isep.ipp.pt/tap-2025 ../../schedule.xsd ">
      {schedules.sortBy(s => (s.orderId.to, s.productNumber, s.start)).map { sched =>
      <TaskSchedule order={sched.orderId.to}
                    productNumber={sched.productNumber.toString}
                    task={sched.taskId.to}
                    start={sched.start.toString}
                    end={sched.end.toString}>
        <PhysicalResources>
          {sched.physicalResourceIds.map(id =>
            <Physical id={id.to}/>
        )}
        </PhysicalResources>
        <HumanResources>
          {sched.humanResourceNames.map(name =>
            <Human name={name}/>
        )}
        </HumanResources>
      </TaskSchedule>
    }}
    </Schedule>

  // The main function to create the schedule
  def create(xml: Elem): Result[Elem] =
    scheduleDataRetriever(xml) match
      case Right((physResources, physTypes, tasks, humanResources, products, orders)) =>
        generateSchedule(physResources, physTypes, tasks, humanResources, products, orders)
      case Left(error) =>
        Left(error)
}