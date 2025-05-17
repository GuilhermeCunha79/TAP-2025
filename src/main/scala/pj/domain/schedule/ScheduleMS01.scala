package pj.domain.schedule

import pj.domain.*
import pj.domain.resources.*
import pj.domain.resources.Types.*
import pj.xml.{XML, XMLToDomain}

import scala.xml.Elem

object ScheduleMS01 extends Schedule:

  def scheduleDataRetriever(xml: Elem): Result[(
    List[PhysicalResource],
      List[PhysicalResourceType],
      List[Task],
      List[HumanResource],
      List[Product],
      List[Order]
    )] =
    for {
      physicalNode <- XML.fromNode(xml, "PhysicalResources")
      physicalResources <- XML.traverse(physicalNode \ "Physical", XMLToDomain.getPhysicalResource)
      physicalTypes = physicalResources.map(_.physical_type).distinct

      tasksNode <- XML.fromNode(xml, "Tasks")
      tasks <- XML.traverse(tasksNode \ "Task", XMLToDomain.getTask(physicalTypes))

      humanNode <- XML.fromNode(xml, "HumanResources")
      humanResources <- XML.traverse(humanNode \ "Human", XMLToDomain.getHumanResource(physicalTypes))

      productsNode <- XML.fromNode(xml, "Products")
      products <- XML.traverse(productsNode \ "Product", XMLToDomain.getProduct(tasks))

      ordersNode <- XML.fromNode(xml, "Orders")
      orders <- XML.traverse(ordersNode \ "Order", XMLToDomain.getOrder(products))
    } yield (physicalResources, physicalTypes, tasks, humanResources, products, orders)


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
    ): Result[List[HumanResourceName]] =
    requiredTypes.foldLeft[Result[(List[HumanResourceName], Set[HumanResourceName])]](Right((Nil, Set.empty))) {
      case (accResult, requiredType) => for {
        (assignedNames, usedNames) <- accResult
        human <- availableHumans
          .find(hr => hr.physicalResourceTypes.contains(requiredType) && !usedNames.contains(hr.name))
          .toRight(DomainError.ResourceUnavailable(taskId.to, requiredType.to))
      } yield (human.name :: assignedNames, usedNames + human.name)
    }.map(_._1.reverse)


  private def generateSchedule(
      physicalResources: List[PhysicalResource],
      physicalTypes: List[PhysicalResourceType],
      allTasks: List[Task],
      humanResources: List[HumanResource],
      allProducts: List[Product],
      allOrders: List[Order]
    ): Result[Elem] =
    val initial: Result[(List[TaskSchedule], Int)] = Right((Nil, 0))

    val result = allOrders.foldLeft(initial) { (acc, order) =>
      for {
        (scheduledSoFar, globalTime) <- acc
        (newSchedules, newTime) <- scheduleOrder(
          order, globalTime, scheduledSoFar, allProducts, allTasks, physicalResources, humanResources
        )
      } yield (newSchedules, newTime)
    }

    result.map((schedules, _) => toXml(schedules))


  private def scheduleOrder(
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


  private def scheduleProduct(
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


  private def scheduleTask(
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


  private def toXml(schedules: List[TaskSchedule]): Elem =
    <Schedule xmlns="http://www.dei.isep.ipp.pt/tap-2025"
              xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
              xsi:schemaLocation="http://www.dei.isep.ipp.pt/tap-2025 ../../schedule.xsd ">
      {schedules.sortBy(s => (s.orderId.to, s.productNumber.to, s.start.to)).map { sched =>
      <TaskSchedule order={sched.orderId.to}
                    productNumber={sched.productNumber.to.toString}
                    task={sched.taskId.to}
                    start={sched.start.to.toString}
                    end={sched.end.to.toString}>
        <PhysicalResources>
          {sched.physicalResourceIds.map(id =>
            <Physical id={id.to}/>
        )}
        </PhysicalResources>
        <HumanResources>
          {sched.humanResourceNames.map(name =>
            <Human name={name.to}/>
        )}
        </HumanResources>
      </TaskSchedule>
    }}
    </Schedule>

  def create(xml: Elem): Result[Elem] =
    scheduleDataRetriever(xml) match
      case Right((physResources, physTypes, tasks, humanResources, products, orders)) =>
        generateSchedule(physResources, physTypes, tasks, humanResources, products, orders)
      case Left(error) =>
        Left(error)