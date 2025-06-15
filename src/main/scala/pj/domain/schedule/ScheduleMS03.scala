package pj.domain.schedule

import pj.domain.*
import pj.domain.resources.*
import pj.domain.resources.Types.*
import pj.io.FileIO
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

    for {
      _ <- validateResourceRequirements(tasks, physicalResources, humanResources)

      taskMap = tasks.map(t => t.id -> t).toMap
      productTaskMap = products.map(p => p.id -> p.tasksList).toMap
      allProductInstances = createAllProductInstances(orders, productTaskMap, taskMap)
      initialReadyTasks = getInitialReadyTasks(allProductInstances)
      initialResourceAvailability = initializeResourceAvailability(physicalResources, humanResources)
      initialState = SchedulingState(
        readyTasks = initialReadyTasks,
        resourceAvailability = initialResourceAvailability,
        schedules = List.empty,
        productProgress = Map.empty
      )
      result <- scheduleAllTasks(initialState, allProductInstances, physicalResources, humanResources)
      _ = println(s"Final scheduling result: $result")
    } yield result.schedules.reverse


  def validateTask(
    task: Task,
    physicalResourceCounts: Map[PhysicalResourceType, Int],
    humanResourceTypes: Set[PhysicalResourceType]
  ): Result[Unit] =

    val requiredResourceCounts = task.physicalResourceTypes.groupBy(identity).view.mapValues(_.size)

    val resourceValidation = requiredResourceCounts.find { case (resourceType, requiredCount) =>
      val availableCount = physicalResourceCounts.getOrElse(resourceType, 0)
      availableCount < requiredCount
    } match
      case Some((resourceType, requiredCount)) =>
        Left(DomainError.ImpossibleSchedule)
      case None => Right(())

    resourceValidation.flatMap { _ =>

      task.physicalResourceTypes.find { resourceType =>
        !humanResourceTypes.contains(resourceType)
      } match
        case Some(resourceType) =>
          Left(DomainError.ImpossibleSchedule)
        case None => Right(())
    }

  def validateResourceRequirements(
    tasks: List[Task],
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): Result[Unit] =
    
    val physicalResourceCounts = physicalResources.groupBy(_.physical_type).view.mapValues(_.size).toMap
    val humanResourceTypes = humanResources.flatMap(_.physicalResourceTypes).toSet

    tasks.foldLeft[Result[Unit]](Right(())) { (acc, task) =>
      acc.flatMap(_ => validateTask(task, physicalResourceCounts, humanResourceTypes))
    }


  def createAllProductInstances(
    orders: List[Order],
    productTaskMap: Map[ProductId, List[TaskId]],
    taskMap: Map[TaskId, Task]
  ): List[TaskInfo] =
    for {
      order <- orders
      tasksList <- productTaskMap.get(order.productId).toList
      productNumber <- 1 to order.quantity.to
      (taskId, taskIndex) <- tasksList.zipWithIndex
      task <- taskMap.get(taskId).toList
      productNum <- ProductNumber.from(productNumber).toOption
      earliest <- EarliestStartTime.from(0).toOption
      productTaskIndex <- ProductTaskIndex.from(taskIndex).toOption
    } yield TaskInfo(order.id, productNum, taskId, task, earliest, productTaskIndex)

  def getInitialReadyTasks(allTasks: List[TaskInfo]): List[TaskInfo] =
    allTasks.filter(_.productTaskIndex.to == 0)


  def initializeResourceAvailability(
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): Map[String, Int] =
    val resourceKeys = physicalResources.map(r => s"PHYS_${r.id.to}") ++
      humanResources.map(r => s"HUMAN_${r.name.to}")
    resourceKeys.map(_ -> 0).toMap


  def scheduleAllTasks(
    state: SchedulingState,
    allTasks: List[TaskInfo],
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): Result[SchedulingState] =

    state.readyTasks match
      case Nil => Right(state)
      case _ =>
        for {
          nextState <- scheduleNextBatch(state, physicalResources, humanResources)
          updatedTasks = updateReadyTasks(nextState, allTasks)
          finalState <- (updatedTasks.schedules.lengthIs > state.schedules.length) match
            case true =>
              scheduleAllTasks(updatedTasks, allTasks, physicalResources, humanResources)
            case false =>
              advanceTimeAndRetry(state, physicalResources, humanResources, allTasks)
        } yield finalState


  def advanceTimeAndRetry(
    state: SchedulingState,
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource],
    allTasks: List[TaskInfo]
  ): Result[SchedulingState] =
    val minEarliestStart = state.readyTasks.map(_.earliestStart.to).minOption.getOrElse(0)
    val nextResourceTime = state.resourceAvailability.values.filter(_ > minEarliestStart).minOption

    nextResourceTime match
      case Some(nextTime) =>
        val advancedTasks = state.readyTasks.map { t =>
          val newStart = EarliestStartTime.from(nextTime).getOrElse(t.earliestStart)
          t.copy(earliestStart = newStart)
        }
        val advancedState = state.copy(readyTasks = advancedTasks)
        scheduleAllTasks(advancedState, allTasks, physicalResources, humanResources)
      case None =>
        Left(DomainError.ImpossibleSchedule)


  def scheduleNextBatch(
    state: SchedulingState,
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): Result[SchedulingState] =

    val currentTime = math.max(findNextAvailableTime(state),
      state.readyTasks.map(_.earliestStart.to).minOption.getOrElse(0))
    val availableAtCurrentTime = state.readyTasks.filter(_.earliestStart.to <= currentTime)

    availableAtCurrentTime.headOption.fold[Result[SchedulingState]](Right(state)) { _ =>
      scheduleMaximumTasksAtTime(state, availableAtCurrentTime, currentTime, physicalResources, humanResources)
    }


  def findNextAvailableTime(state: SchedulingState): Int =
    state.resourceAvailability.values.view.filter(_ > 0).minOption.getOrElse(0)


  def scheduleMaximumTasksAtTime(
    state: SchedulingState,
    candidateTasks: List[TaskInfo],
    currentTime: Int,
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): Result[SchedulingState] =

    val prioritizedTasks = prioritizeTasks(candidateTasks)
    // Try to schedule as many tasks as possible in parallel
    scheduleTasksBatch(state, prioritizedTasks, currentTime, physicalResources, humanResources)


  def prioritizeTasks(tasks: List[TaskInfo]): List[TaskInfo] =
    // Sort by: shortest duration first (for better parallelization)
    tasks.sortBy(task => (
      task.task.time.to, // Shortest tasks first for better resource utilization
      task.task.physicalResourceTypes.size, // Tasks requiring fewer resources first
      task.productTaskIndex.to,
      task.orderId.to,
      task.productNumber.to
    ))
  
  def scheduleTasksBatch(
    state: SchedulingState,
    tasks: List[TaskInfo],
    currentTime: Int,
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): Result[SchedulingState] =

    // Pre-compute resource availability for this time slot
    val resourcesAtTime = getAvailableResourcesAtTime(physicalResources, humanResources, state.resourceAvailability, currentTime)

    scheduleBatchRecursively(state, tasks, currentTime, resourcesAtTime._1, resourcesAtTime._2, Set.empty, Set.empty)
  
  
  def getAvailableResourcesAtTime(
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource],
    availability: Map[String, Int],
    currentTime: Int
  ): (List[PhysicalResource], List[HumanResource]) =
    val availablePhysical = physicalResources.filter { res =>
      availability.getOrElse(s"PHYS_${res.id.to}", 0) <= currentTime
    }
    val availableHuman = humanResources.filter { res =>
      availability.getOrElse(s"HUMAN_${res.name.to}", 0) <= currentTime
    }
    (availablePhysical, availableHuman)


  def scheduleBatchRecursively(
    state: SchedulingState,
    tasks: List[TaskInfo],
    currentTime: Int,
    availablePhysical: List[PhysicalResource],
    availableHuman: List[HumanResource],
    usedPhysicalIds: Set[PhysicalResourceId],
    usedHumanNames: Set[HumanResourceName]
  ): Result[SchedulingState] =

    tasks match
      case Nil => Right(state)
      case taskInfo :: remainingTasks =>
        tryScheduleTaskWithAvailableResources(
          taskInfo, currentTime, availablePhysical, availableHuman, usedPhysicalIds, usedHumanNames
        ) match
          case Right((physicalIds, humanNames)) =>
            // Successfully allocated resources, create schedule and continue
            createTaskSchedule(taskInfo, currentTime, physicalIds, humanNames).flatMap { schedule =>
              val newState = updateStateAfterScheduling(state, taskInfo, schedule, currentTime)
              val updatedUsedPhysical = usedPhysicalIds ++ physicalIds.toSet
              val updatedUsedHuman = usedHumanNames ++ humanNames.toSet
              val filteredRemaining = remainingTasks.filterNot(t =>
                t.orderId == taskInfo.orderId &&
                  t.productNumber == taskInfo.productNumber &&
                  t.taskId == taskInfo.taskId
              )
              scheduleBatchRecursively(
                newState, filteredRemaining, currentTime, availablePhysical, availableHuman,
                updatedUsedPhysical, updatedUsedHuman
              )
            }
          case Left(_) =>
            // Cannot schedule this task, try next one
            scheduleBatchRecursively(
              state, remainingTasks, currentTime, availablePhysical, availableHuman,
              usedPhysicalIds, usedHumanNames
            )


  def tryScheduleTaskWithAvailableResources(
    taskInfo: TaskInfo,
    currentTime: Int,
    availablePhysical: List[PhysicalResource],
    availableHuman: List[HumanResource],
    usedPhysicalIds: Set[PhysicalResourceId],
    usedHumanNames: Set[HumanResourceName]
  ): Result[(List[PhysicalResourceId], List[HumanResourceName])] =

    for {
      physicalIds <- allocateResourcesFromAvailable(
        taskInfo.task.physicalResourceTypes, availablePhysical, usedPhysicalIds
      )
      humanNames <- allocateHumansFromAvailable(
        taskInfo.task.physicalResourceTypes, availableHuman, usedHumanNames
      )
    } yield (physicalIds, humanNames)


  def allocateResourcesFromAvailable(
    requiredTypes: List[PhysicalResourceType],
    availablePhysical: List[PhysicalResource],
    usedIds: Set[PhysicalResourceId]
  ): Result[List[PhysicalResourceId]] =
    allocatePhysicalRecursively(requiredTypes, availablePhysical, List.empty, usedIds)

  
  def allocatePhysicalRecursively(
    requiredTypes: List[PhysicalResourceType],
    availablePhysical: List[PhysicalResource],
    allocatedIds: List[PhysicalResourceId],
    usedIds: Set[PhysicalResourceId]
  ): Result[List[PhysicalResourceId]] =
    requiredTypes match
      case Nil => Right(allocatedIds.reverse)
      case requiredType :: remainingTypes =>
        availablePhysical.find { res =>
          res.physical_type == requiredType && !usedIds.contains(res.id)
        } match
          case Some(resource) =>
            allocatePhysicalRecursively(
              remainingTypes, availablePhysical, resource.id :: allocatedIds, usedIds + resource.id
            )
          case None =>
            Left(DomainError.ImpossibleSchedule)


  def allocateHumansFromAvailable(
    requiredTypes: List[PhysicalResourceType],
    availableHuman: List[HumanResource],
    usedNames: Set[HumanResourceName]
  ): Result[List[HumanResourceName]] =
    allocateHumansRecursively(requiredTypes, availableHuman, List.empty, usedNames)

  def allocateHumansRecursively(
    requiredTypes: List[PhysicalResourceType],
    availableHuman: List[HumanResource],
    allocatedNames: List[HumanResourceName],
    usedNames: Set[HumanResourceName]
  ): Result[List[HumanResourceName]] =
    requiredTypes match
      case Nil => Right(allocatedNames.reverse)
      case requiredType :: remainingTypes =>
        availableHuman.find { hr =>
          hr.physicalResourceTypes.contains(requiredType) && !usedNames.contains(hr.name)
        } match
          case Some(human) =>
            allocateHumansRecursively(
              remainingTypes, availableHuman, human.name :: allocatedNames, usedNames + human.name
            )
          case None =>
            Left(DomainError.ImpossibleSchedule)


  def allocatePhysicalResourcesAtTime(
    task: Task,
    physicalResources: List[PhysicalResource],
    availability: Map[String, Int],
    currentTime: Int
  ): Result[List[PhysicalResourceId]] =
    allocateResourcesRecursively(task.physicalResourceTypes, physicalResources, availability, currentTime, List.empty, Set.empty)

  def allocateResourcesRecursively(
    requiredTypes: List[PhysicalResourceType],
    physicalResources: List[PhysicalResource],
    availability: Map[String, Int],
    currentTime: Int,
    allocatedIds: List[PhysicalResourceId],
    usedIds: Set[PhysicalResourceId]
  ): Result[List[PhysicalResourceId]] =
    requiredTypes match
      case Nil => Right(allocatedIds.reverse)
      case requiredType :: remainingTypes =>
        physicalResources.find { res =>
          res.physical_type == requiredType &&
            !usedIds.contains(res.id) &&
            availability.getOrElse(s"PHYS_${res.id.to}", 0) <= currentTime
        } match
          case Some(resource) =>
            allocateResourcesRecursively(
              remainingTypes,
              physicalResources,
              availability,
              currentTime,
              resource.id :: allocatedIds,
              usedIds + resource.id
            )
          case None =>
            Left(DomainError.ImpossibleSchedule)

  def allocateHumanResourcesAtTime(
    task: Task,
    humanResources: List[HumanResource],
    availability: Map[String, Int],
    currentTime: Int
  ): Result[List[HumanResourceName]] =
    allocateHumansRecursively(task.physicalResourceTypes, humanResources, availability, currentTime, List.empty, Set.empty)

  def allocateHumansRecursively(
    requiredTypes: List[PhysicalResourceType],
    humanResources: List[HumanResource],
    availability: Map[String, Int],
    currentTime: Int,
    allocatedNames: List[HumanResourceName],
    usedNames: Set[HumanResourceName]
  ): Result[List[HumanResourceName]] =
    requiredTypes match
      case Nil => Right(allocatedNames.reverse)
      case requiredType :: remainingTypes =>
        humanResources.find { hr =>
          hr.physicalResourceTypes.contains(requiredType) &&
            !usedNames.contains(hr.name) &&
            availability.getOrElse(s"HUMAN_${hr.name.to}", 0) <= currentTime
        } match
          case Some(human) =>
            allocateHumansRecursively(
              remainingTypes,
              humanResources,
              availability,
              currentTime,
              human.name :: allocatedNames,
              usedNames + human.name
            )
          case None =>
            Left(DomainError.ImpossibleSchedule)

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

    val updatedAvailability = updateResourceAvailability(
      state.resourceAvailability, schedule, endTime
    )

    val progressKey = (scheduledTask.orderId, scheduledTask.productNumber)
    val currentProgress = state.productProgress.getOrElse(progressKey, 0)
    val updatedProgress = state.productProgress.updated(progressKey, currentProgress + 1)

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
    val allUpdates = schedule.physicalResourceIds.map(id => s"PHYS_${id.to}" -> endTime) ++
      schedule.humanResourceNames.map(name => s"HUMAN_${name.to}" -> endTime)
    availability ++ allUpdates


  def updateReadyTasks(
    state: SchedulingState,
    allTasks: List[TaskInfo]
  ): SchedulingState =
    
    val scheduledTaskKeys = state.schedules.map(s => (s.orderId, s.productNumber, s.taskId)).toSet
    val readyTaskKeys = state.readyTasks.map(rt => (rt.orderId, rt.productNumber, rt.taskId)).toSet

    val newReadyTasks = allTasks.filter { task =>
      val progressKey = (task.orderId, task.productNumber)
      val taskKey = (task.orderId, task.productNumber, task.taskId)
      val completedTasks = state.productProgress.getOrElse(progressKey, 0)

      task.productTaskIndex.to == completedTasks &&
        !readyTaskKeys.contains(taskKey) &&
        !scheduledTaskKeys.contains(taskKey)
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
      {schedules.sortBy(_.start.to).map { sched =>
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
      outputXml = toXml(schedules)
      _ = FileIO.save("output.xml", outputXml)
    } yield toXml(schedules)
