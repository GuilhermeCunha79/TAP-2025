package pj.domain.schedule

import pj.domain.*
import pj.domain.resources.*
import pj.domain.resources.Types.*
import pj.io.FileIO
import pj.xml.{XML, XMLToDomain}

import scala.xml.Elem

object ScheduleMS01 extends Schedule:

  def allocatePhysicalResources(
       taskId: TaskId,
       requiredTypes: List[PhysicalResourceType],
       availableResources: List[PhysicalResource]
     ): Result[List[PhysicalResourceId]] =
    requiredTypes.foldLeft[Result[(List[PhysicalResourceId], Set[PhysicalResourceId])]](Right((Nil, Set.empty))) {
      case (accResult, requiredType) => for {
        (assignedIds, usedIds) <- accResult
        resource <- availableResources
          .find(res => res.physical_type == requiredType && !usedIds.contains(res.id))
          .toRight(DomainError.ResourceUnavailable(taskId.to, requiredType.to))
      } yield (resource.id :: assignedIds, usedIds + resource.id)
    }.map(_._1.reverse)


  def allocateHumanResources(
      taskId: TaskId,
      requiredTypes: List[PhysicalResourceType],
      availableHumans: List[HumanResource]
    ): Result[List[HumanResourceId]] =
    requiredTypes.foldLeft[Result[(List[HumanResourceId], Set[HumanResourceId])]](Right((Nil, Set.empty))) {
      case (accResult, requiredType) => for {
        (assignedNames, usedNames) <- accResult
        human <- availableHumans
          .find(hr => hr.physicalResourceTypes.contains(requiredType) && !usedNames.contains(hr.id))
          .toRight(DomainError.ResourceUnavailable(taskId.to, requiredType.to))
      } yield (human.id :: assignedNames, usedNames + human.id)
    }.map(_._1.reverse)


  def generateSchedule(
      orders: List[Order],
      products: List[Product],
      tasks: List[Task],
      humanResources: List[HumanResource],
      physicalResources: List[PhysicalResource]
    ): Result[List[TaskSchedule]] =
    val result = orders.foldLeft(Right((List.empty[TaskSchedule], 0)): Result[(List[TaskSchedule], Int)]) { (acc, order) =>
      for {
        (scheduledSoFar, globalTime) <- acc
        (newSchedules, newTime) <- scheduleOrder(
          order, globalTime, scheduledSoFar, products, tasks, physicalResources, humanResources
        )
      } yield (newSchedules, newTime)
    }

    result.map(_._1)


  def scheduleOrder(
       order: Order,
       startTime: Int,
       scheduled: List[TaskSchedule],
       allProducts: List[Product],
       allTasks: List[Task],
       physicalResources: List[PhysicalResource],
       humanResources: List[HumanResource]
     ): Result[(List[TaskSchedule], Int)] =
    for {
      product <- allProducts
        .find(_.id == order.productId)
        .toRight(DomainError.ProductDoesNotExist(order.productId.to))

      result <- (1 to order.quantity.to).foldLeft[Result[(List[TaskSchedule], Int)]](Right((scheduled, startTime))):
        (acc, instanceNum) =>
          for {
            (scheduledSoFar, timeSoFar) <- acc
            (newTasks, newTime) <- scheduleProduct(
              order.id, product, instanceNum, timeSoFar, scheduledSoFar, allTasks, physicalResources, humanResources
            )
          } yield (newTasks, newTime)
    } yield result


  def scheduleProduct(
       orderId: OrderId,
       product: Product,
       productInstance: Int,
       startTime: Int,
       scheduled: List[TaskSchedule],
       allTasks: List[Task],
       physicalResources: List[PhysicalResource],
       humanResources: List[HumanResource]
     ): Result[(List[TaskSchedule], Int)] =
    product.tasksList.foldLeft[Result[(List[TaskSchedule], Int)]](Right((scheduled, startTime))):
      (acc, taskId) =>
        for {
          (scheduledSoFar, currentTime) <- acc
          task <- allTasks.find(_.id == taskId)
            .toRight(DomainError.TaskDoesNotExist(taskId.to))
          (scheduledTask, endTime) <- scheduleTask(
            orderId, task, productInstance, currentTime, physicalResources, humanResources
          )
        } yield (scheduledTask :: scheduledSoFar, endTime)


  def scheduleTask(
      orderId: OrderId,
      task: Task,
      productInstance: Int,
      startTime: Int,
      physicalResources: List[PhysicalResource],
      humanResources: List[HumanResource]
    ): Result[(TaskSchedule, Int)] = for {
    physical <- allocatePhysicalResources(task.id, task.physicalResourceTypes, physicalResources)
    humans <- allocateHumanResources(task.id, task.physicalResourceTypes, humanResources)
    productNum <- ProductNumber.from(productInstance)
    start <- TaskScheduleTime.from(startTime)
    end <- TaskScheduleTime.from(startTime + task.time.to)
  } yield
    (TaskSchedule(orderId, productNum, task.id, start, end, physical, humans), end.to)


  private def getHumanNameById(
                                humanId: HumanResourceId,
                                humanResources: List[HumanResource]
                              ): String =
    humanResources.find(_.id == humanId).map(_.name.to).getOrElse(humanId.to)

  def create(xml: Elem): Result[Elem] =
    for {
      (orders, products, tasks, humanResources, physicalResources) <- Shared.scheduleDataRetriever(xml)
      schedules <- generateSchedule(orders, products, tasks, humanResources, physicalResources)
      outputXml = Shared.toXml(schedules, humanResources)
      _ = FileIO.save("output.xml", outputXml)
    } yield outputXml

  def create(xml: Elem, fileName: String): Result[Elem] =
    for {
      (orders, products, tasks, humanResources, physicalResources) <- Shared.scheduleDataRetriever(xml)
      schedules <- generateSchedule(orders, products, tasks, humanResources, physicalResources)
      outputXml = Shared.toXml(schedules, humanResources)
      _ = FileIO.save(fileName, outputXml)
    } yield outputXml