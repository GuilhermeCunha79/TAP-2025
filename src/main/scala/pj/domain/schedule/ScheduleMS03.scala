package pj.domain.schedule

import pj.domain.*
import pj.domain.resources.*
import pj.domain.resources.Types.*
import pj.xml.{XML, XMLToDomain}

import scala.xml.Elem

object ScheduleMS03 extends Schedule:

  def generateSchedule(
    orders: List[Order],
    products: List[Product],
    tasks: List[Task],
    humanResources: List[HumanResource],
    physicalResources: List[PhysicalResource]
  ): Result[List[TaskSchedule]] =

    val allProductInstances = createAllProductInstances(orders, products, tasks)

    val initialReadyTasks = getInitialReadyTasks(allProductInstances)

    val initialResourceAvailability = initializeResourceAvailability(physicalResources, humanResources)

    val initialState = SchedulingState(
      readyTasks = initialReadyTasks,
      resourceAvailability = initialResourceAvailability,
      schedules = List.empty,
      productProgress = Map.empty
    )

    val result = scheduleAllTasks(initialState, allProductInstances, physicalResources, humanResources, tasks)
    println(s"Final scheduling result: $result")

    result.map(_.schedules.reverse)


  def createAllProductInstances(
   orders: List[Order],
   products: List[Product],
   tasks: List[Task]
  ): List[TaskInfo] =
    for {
      order <- orders
      product <- products if product.id == order.productId
      productNumber <- 1 to order.quantity.to
      (taskId, taskIndex) <- product.tasksList.zipWithIndex
      task <- tasks if task.id == taskId
      productNum <- ProductNumber.from(productNumber).toOption
    } yield TaskInfo(order.id, productNum, taskId, task, 0, taskIndex)

  def getInitialReadyTasks(allTasks: List[TaskInfo]): List[TaskInfo] =
    allTasks.filter(_.productTaskIndex == 0)

  def initializeResourceAvailability(
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): Map[String, Int] =
    val physicalAvailability = physicalResources.map(r => s"PHYS_${r.id.to}" -> 0).toMap
    val humanAvailability = humanResources.map(r => s"HUMAN_${r.name.to}" -> 0).toMap
    physicalAvailability ++ humanAvailability

  def scheduleAllTasks(
    state: SchedulingState,
    allTasks: List[TaskInfo],
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource],
    tasks: List[Task]
  ): Result[SchedulingState] =

    if (state.readyTasks.isEmpty) Right(state)
    else
      for {
        nextState <- scheduleNextBatch(state, physicalResources, humanResources)
        updatedTasks = updateReadyTasks(nextState, allTasks)
        finalState <- scheduleAllTasks(
          updatedTasks, allTasks, physicalResources, humanResources, tasks
        )
      } yield finalState

  def scheduleNextBatch(
    state: SchedulingState,
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): Result[SchedulingState] =

    val currentTime = math.max(findNextAvailableTime(state),
      state.readyTasks.map(_.earliestStart).minOption.getOrElse(0))
    val availableAtCurrentTime = state.readyTasks.filter(_.earliestStart <= currentTime)

    if (availableAtCurrentTime.isEmpty)
      // Advance time to next meaningful point
      val nextTime = state.resourceAvailability.values.filter(_ > currentTime).minOption.getOrElse(currentTime + 1)
      val updatedReadyTasks = state.readyTasks.map(t => t.copy(earliestStart = math.max(t.earliestStart, nextTime)))
      Right(state.copy(readyTasks = updatedReadyTasks))
    else
      scheduleTasksAtTime(state, availableAtCurrentTime, currentTime, physicalResources, humanResources)

  def findNextAvailableTime(state: SchedulingState): Int =
    val resourceTimes = state.resourceAvailability.values.filter(_ > 0)

    resourceTimes.foldLeft(Int.MaxValue)(Math.min) match
      case Int.MaxValue => 0 // If the collection was empty, return 0
      case minValue =>
        minValue


  def scheduleTasksAtTime(
    state: SchedulingState,
    candidateTasks: List[TaskInfo],
    currentTime: Int,
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): Result[SchedulingState] =

    val prioritizedTasks = prioritizeTasks(candidateTasks)
    scheduleTasksRecursively(state, prioritizedTasks, currentTime, physicalResources, humanResources)

  def prioritizeTasks(tasks: List[TaskInfo]): List[TaskInfo] =
    tasks.sortBy(task => (
      task.task.time.to,           // Shorter tasks first
      task.productTaskIndex,       // Earlier tasks in product sequence first
      task.orderId.to,             // Consistent ordering
      task.productNumber.to
    ))

  def scheduleTasksRecursively(
    state: SchedulingState,
    tasks: List[TaskInfo],
    currentTime: Int,
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): Result[SchedulingState] =

    tasks match
      case Nil => Right(state)
      case taskInfo :: remainingTasks =>
        tryScheduleTask(state, taskInfo, currentTime, physicalResources, humanResources) match
          case Right(newState) =>
            val updatedRemaining = remainingTasks.filterNot(t =>
              t.orderId == taskInfo.orderId &&
                t.productNumber == taskInfo.productNumber &&
                t.taskId == taskInfo.taskId
            )
            scheduleTasksRecursively(newState, updatedRemaining, currentTime, physicalResources, humanResources)
          case Left(_) =>
            // If task cannot be scheduled now, defer it
            val deferredTask = taskInfo.copy(earliestStart = currentTime + 1)
            val updatedState = state.copy(
              readyTasks = state.readyTasks.map(t =>
                if (t.orderId == taskInfo.orderId &&
                  t.productNumber == taskInfo.productNumber &&
                  t.taskId == taskInfo.taskId)
                  deferredTask
                else t
              )
            )
            scheduleTasksRecursively(updatedState, remainingTasks, currentTime, physicalResources, humanResources)

  def tryScheduleTask(
    state: SchedulingState,
    taskInfo: TaskInfo,
    currentTime: Int,
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): Result[SchedulingState] =
  
    for {
      physicalIds <- allocatePhysicalResourcesAtTime(taskInfo.task, physicalResources, state.resourceAvailability, currentTime)
      humanNames <- allocateHumanResourcesAtTime(taskInfo.task, humanResources, state.resourceAvailability, currentTime)
      schedule <- createTaskSchedule(taskInfo, currentTime, physicalIds, humanNames)
      newState = updateStateAfterScheduling(state, taskInfo, schedule, currentTime)
    } yield newState

  def allocatePhysicalResourcesAtTime(
    task: Task,
    physicalResources: List[PhysicalResource],
    availability: Map[String, Int],
    currentTime: Int
  ): Result[List[PhysicalResourceId]] =

    task.physicalResourceTypes.foldLeft[Result[(List[PhysicalResourceId], Set[PhysicalResourceId])]](
      Right((Nil, Set.empty))
    ) { case (accResult, requiredType) =>
      for {
        (assignedIds, usedIds) <- accResult
        resource <- physicalResources
          .find(res => res.physical_type == requiredType &&
            !usedIds.contains(res.id) &&
            availability.getOrElse(s"PHYS_${res.id.to}", 0) <= currentTime)
          .toRight(DomainError.ResourceUnavailable(task.id.to, requiredType.to))
      } yield (resource.id :: assignedIds, usedIds + resource.id)
    }.map(_._1.reverse)

  def allocateHumanResourcesAtTime(
    task: Task,
    humanResources: List[HumanResource],
    availability: Map[String, Int],
    currentTime: Int
  ): Result[List[HumanResourceName]] =

    task.physicalResourceTypes.foldLeft[Result[(List[HumanResourceName], Set[HumanResourceName])]](
      Right((Nil, Set.empty))
    ) { case (accResult, requiredType) =>
      for {
        (assignedNames, usedNames) <- accResult
        human <- humanResources
          .find(hr => hr.physicalResourceTypes.contains(requiredType) &&
            !usedNames.contains(hr.name) &&
            availability.getOrElse(s"HUMAN_${hr.name.to}", 0) <= currentTime)
          .toRight(DomainError.ResourceUnavailable(task.id.to, requiredType.to))
      } yield (human.name :: assignedNames, usedNames + human.name)
    }.map(_._1.reverse)

  def createTaskSchedule(
    taskInfo: TaskInfo,
    startTime: Int,
    physicalIds: List[PhysicalResourceId],
    humanNames: List[HumanResourceName]
  ): Result[TaskSchedule] =

    for {
      start <- TaskScheduleTime.from(startTime)
      end <- TaskScheduleTime.from(startTime + taskInfo.task.time.to)
    } yield TaskSchedule(
      taskInfo.orderId,
      taskInfo.productNumber,
      taskInfo.taskId,
      start,
      end,
      physicalIds,
      humanNames
    )
  
  def updateStateAfterScheduling(
    state: SchedulingState,
    scheduledTask: TaskInfo,
    schedule: TaskSchedule,
    currentTime: Int
  ): SchedulingState =

      val endTime = currentTime + scheduledTask.task.time.to

      // Update resource availability
      val updatedAvailability = updateResourceAvailability(
        state.resourceAvailability, schedule, endTime
      )

      // Update product progress
      val progressKey = (scheduledTask.orderId, scheduledTask.productNumber)
      val currentProgress = state.productProgress.getOrElse(progressKey, 0)
      val updatedProgress = state.productProgress.updated(progressKey, currentProgress + 1)

      // Remove scheduled task from ready tasks
      val remainingTasks = state.readyTasks.filterNot(t =>
        t.orderId == scheduledTask.orderId &&
          t.productNumber == scheduledTask.productNumber &&
          t.taskId == scheduledTask.taskId
      )

      state.copy(
        readyTasks = remainingTasks,
        resourceAvailability = updatedAvailability,
        schedules = schedule :: state.schedules,
        productProgress = updatedProgress
      )

  def updateResourceAvailability(
    availability: Map[String, Int],
    schedule: TaskSchedule,
    endTime: Int
  ): Map[String, Int] = 

    val physicalUpdates = schedule.physicalResourceIds.map(id => s"PHYS_${id.to}" -> endTime)
    val humanUpdates = schedule.humanResourceNames.map(name => s"HUMAN_${name.to}" -> endTime)
    
    availability ++ physicalUpdates ++ humanUpdates
    
  def updateReadyTasks(
    state: SchedulingState,
    allTasks: List[TaskInfo]
  ): SchedulingState =
  
    val newReadyTasks = allTasks.filter { task =>
      val progressKey = (task.orderId, task.productNumber)
      val completedTasks = state.productProgress.getOrElse(progressKey, 0)
    
      // Task is ready if it's the next in sequence and not already scheduled
      task.productTaskIndex == completedTasks &&
        !state.readyTasks.exists(rt =>
          rt.orderId == task.orderId &&
            rt.productNumber == task.productNumber &&
            rt.taskId == task.taskId
        ) &&
        !state.schedules.exists(s =>
          s.orderId == task.orderId &&
            s.productNumber == task.productNumber &&
            s.taskId == task.taskId
        )
    }
    
    state.copy(readyTasks = state.readyTasks ++ newReadyTasks)


  def scheduleDataRetriever(xml: Elem): Result[(
    List[Order],
    List[Product],
    List[Task],
    List[HumanResource],
    List[PhysicalResource]
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
    } yield (orders, products, tasks, humanResources, physicalResources)

  def toXml(schedules: List[TaskSchedule]): Elem =
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
    for {
      (orders, products, tasks, humanResources, physicalResources) <- scheduleDataRetriever(xml)
      schedules <- generateSchedule(orders, products, tasks, humanResources, physicalResources)
    } yield toXml(schedules)
