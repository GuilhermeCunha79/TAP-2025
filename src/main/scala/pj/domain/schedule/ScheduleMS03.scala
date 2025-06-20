package pj.domain.schedule

import pj.domain.*
import pj.domain.resources.*
import pj.domain.resources.Types.*
import pj.io.FileIO

import scala.annotation.tailrec
import scala.xml.Elem

object ScheduleMS03 extends Schedule:

  /**
   * Generates a complete schedule for all tasks based on orders, products, and available resources
   */
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
      initialState = createInitialState(allProductInstances, physicalResources, humanResources)
      result <- scheduleAllTasks(initialState, allProductInstances, physicalResources, humanResources)

    } yield result.schedules.reverse

  /**
   * Creates the initial scheduling state with ready tasks and resource availability
   */
  def createInitialState(
    allProductInstances: List[TaskInfo],
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): SchedulingState =
    val initialReadyTasks = allProductInstances.filter(_.productTaskIndex.to == 0)
    val initialResourceAvailability = (
      physicalResources.map(r => {r.id.to} -> 0) ++
        humanResources.map(r => {r.id.to} -> 0)
      ).toMap

    SchedulingState(
      readyTasks = initialReadyTasks,
      resourceAvailability = initialResourceAvailability,
      schedules = List.empty,
      productProgress = Map.empty
    )

  /**
   * Validates if all required resources for tasks are available in sufficient quantities
   */
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

  /**
   * Validates requirements for a single task against available resources
   */
  def validateTask(
    task: Task,
    physicalResourceCounts: Map[PhysicalResourceType, Int],
    humanResourceTypes: Set[PhysicalResourceType]
  ): Result[Unit] =
    val requiredResourceCounts = task.physicalResourceTypes.groupBy(identity).view.mapValues(_.size)

    requiredResourceCounts.find { case (resourceType, requiredCount) =>
      physicalResourceCounts.getOrElse(resourceType, 0) < requiredCount
    }.fold[Result[Unit]](Right(())) { _ =>
      Left(DomainError.ImpossibleSchedule)
    }.flatMap { _ =>
      task.physicalResourceTypes.find(!humanResourceTypes.contains(_))
        .fold[Result[Unit]](Right(())) { _ =>
          Left(DomainError.ImpossibleSchedule)
        }
    }

  /**
   * Creates all product instances for all orders, expanding by quantity and task sequence
   */
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

  /**
   * Recursively schedules all tasks until no more tasks are ready or schedulable
   */
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
          finalState <- updatedTasks.schedules.lengthIs > state.schedules.length match
            case true => scheduleAllTasks(updatedTasks, allTasks, physicalResources, humanResources)
            case false => advanceTimeAndRetry(state, physicalResources, humanResources, allTasks)
        } yield finalState

  /**
   * Advances time when no tasks can be scheduled at current time and retries scheduling
   */
  def advanceTimeAndRetry(
    state: SchedulingState,
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource],
    allTasks: List[TaskInfo]
  ): Result[SchedulingState] =
    val minEarliestStart = state.readyTasks.map(_.earliestStart.to).minOption.getOrElse(0)
    val nextResourceTime = state.resourceAvailability.values.filter(_ > minEarliestStart).minOption

    nextResourceTime.fold[Result[SchedulingState]](
      Left(DomainError.ImpossibleSchedule)
    ) { nextTime =>
      val advancedTasks = state.readyTasks.map { t =>
        val newStart = EarliestStartTime.from(nextTime).getOrElse(t.earliestStart)
        t.copy(earliestStart = newStart)
      }
      val advancedState = state.copy(readyTasks = advancedTasks)
      scheduleAllTasks(advancedState, allTasks, physicalResources, humanResources)
    }

  /**
   * Schedules the next batch of tasks that can be executed at the current time
   */
  def scheduleNextBatch(
     state: SchedulingState,
     physicalResources: List[PhysicalResource],
     humanResources: List[HumanResource]
   ): Result[SchedulingState] =
    val currentTime = math.max(
      state.resourceAvailability.values.view.filter(_ > 0).minOption.getOrElse(0),
      state.readyTasks.map(_.earliestStart.to).minOption.getOrElse(0)
    )
    val availableAtCurrentTime = state.readyTasks.filter(_.earliestStart.to <= currentTime)

    availableAtCurrentTime.headOption.fold[Result[SchedulingState]](
      Right(state)
    ) { _ =>
      scheduleMaximumTasksAtTime(state, availableAtCurrentTime, currentTime, physicalResources, humanResources)
    }

  /**
   * Prioritizes tasks by duration (longest first) and resource rarity
   */
  def prioritizeTasks(tasks: List[TaskInfo]): List[TaskInfo] =
    val rarityMap = tasks
      .flatMap(_.task.physicalResourceTypes)
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toMap

    tasks.sortBy(task =>
      (-task.task.time.to, task.task.physicalResourceTypes.map(rarityMap.getOrElse(_, 0)).sum)
    )

  /**
   * Schedules the maximum number of tasks possible at the specified time
   */
  private def scheduleMaximumTasksAtTime(
    state: SchedulingState,
    candidateTasks: List[TaskInfo],
    currentTime: Int,
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): Result[SchedulingState] =
    val eligibleTasks = filterEligibleTasksForTime(candidateTasks, state, currentTime)
    val prioritizedTasks = prioritizeTasks(eligibleTasks)
    val (availablePhysical, availableHuman) = getAvailableResourcesAtTime(
      physicalResources, humanResources, state.resourceAvailability, currentTime
    )

    scheduleBatchRecursively(state, prioritizedTasks, currentTime, availablePhysical, availableHuman, Set.empty, Set.empty)

  /**
   * Gets resources (physical and human) that are available at the specified time
   */
  private def getAvailableResourcesAtTime(
     physicalResources: List[PhysicalResource],
     humanResources: List[HumanResource],
     availability: Map[String, Int],
     currentTime: Int
   ): (List[PhysicalResource], List[HumanResource]) =
    val availablePhysical = physicalResources.filter { res =>
      availability.getOrElse({res.id.to}, 0) <= currentTime
    }
    val availableHuman = humanResources.filter { res =>
      availability.getOrElse({res.id.to}, 0) <= currentTime
    }
    (availablePhysical, availableHuman)

  /**
   * Recursively schedules a batch of tasks, attempting each one sequentially
   */
  private def scheduleBatchRecursively(
    state: SchedulingState,
    tasks: List[TaskInfo],
    currentTime: Int,
    availablePhysical: List[PhysicalResource],
    availableHuman: List[HumanResource],
    usedPhysicalIds: Set[PhysicalResourceId],
    usedHumanIds: Set[HumanResourceId]
  ): Result[SchedulingState] =
    tasks match
      case Nil => Right(state)
      case taskInfo :: remainingTasks =>
        tryScheduleTask(taskInfo, currentTime, availablePhysical, availableHuman, usedPhysicalIds, usedHumanIds)
          .fold(
            _ => scheduleBatchRecursively(
              state, remainingTasks, currentTime, availablePhysical, availableHuman,
              usedPhysicalIds, usedHumanIds
            ),
            { case (physicalIds, humanIds) =>
              createTaskSchedule(taskInfo, currentTime, physicalIds, humanIds).flatMap { schedule =>
                val newState = updateStateAfterScheduling(state, taskInfo, schedule, currentTime)
                val updatedUsedPhysical = usedPhysicalIds ++ physicalIds.toSet
                val updatedUsedHuman = usedHumanIds ++ humanIds.toSet
                val filteredRemaining = remainingTasks.filterNot(isMatchingTask(_, taskInfo))

                scheduleBatchRecursively(
                  newState, filteredRemaining, currentTime, availablePhysical, availableHuman,
                  updatedUsedPhysical, updatedUsedHuman
                )
              }
            }
          )

  /**
   * Attempts to allocate resources for a specific task
   */
  def tryScheduleTask(
   taskInfo: TaskInfo,
   currentTime: Int,
   availablePhysical: List[PhysicalResource],
   availableHuman: List[HumanResource],
   usedPhysicalIds: Set[PhysicalResourceId],
   usedHumanIds: Set[HumanResourceId]
 ): Result[(List[PhysicalResourceId], List[HumanResourceId])] =
    for {
      physicalIds <- allocateResources(taskInfo.task.physicalResourceTypes, availablePhysical, usedPhysicalIds)
      humanIds <- allocateHumans(taskInfo.task.physicalResourceTypes, availableHuman, usedHumanIds)
    } yield (physicalIds, humanIds)

  /**
   * Checks if two tasks are identical (same order, product number, and task ID)
   */
  def isMatchingTask(t1: TaskInfo, t2: TaskInfo): Boolean =
    t1.orderId == t2.orderId && t1.productNumber == t2.productNumber && t1.taskId == t2.taskId

  /**
   * Generic function to allocate resources based on specific criteria
   */
  private def allocateResources[T, R](
    requiredTypes: List[PhysicalResourceType],
    availableResources: List[T],
    usedIds: Set[R],
    extractId: T => R,
    matchesType: (T, PhysicalResourceType) => Boolean
  ): Result[List[R]] =
    allocateResourcesLoop(requiredTypes.sortBy(t =>
      availableResources.count(res => matchesType(res, t) && !usedIds.contains(extractId(res)))
    ), availableResources, List.empty, usedIds, extractId, matchesType)

  /**
   * Generic recursive loop for sequential resource allocation
   */
  @tailrec
  private def allocateResourcesLoop[T, R](
    remaining: List[PhysicalResourceType],
    availableResources: List[T],
    allocated: List[R],
    used: Set[R],
    extractId: T => R,
    matchesType: (T, PhysicalResourceType) => Boolean
  ): Result[List[R]] = remaining match
    case Nil => Right(allocated.reverse)

    case requiredType :: rest =>
      availableResources.find(res =>
        matchesType(res, requiredType) && !used.contains(extractId(res))
      ) match
        case Some(resource) =>
          val id = extractId(resource)
          allocateResourcesLoop(rest, availableResources, id :: allocated, used + id, extractId, matchesType)
        case None =>
          Left(DomainError.ImpossibleSchedule)


  /**
   * Allocates physical resources for a task
   */
  private def allocateResources(
   requiredTypes: List[PhysicalResourceType],
   availablePhysical: List[PhysicalResource],
   usedIds: Set[PhysicalResourceId]
  ): Result[List[PhysicalResourceId]] =
    allocateResources(
      requiredTypes,
      availablePhysical,
      usedIds,
      (res: PhysicalResource) => res.id,
      (res: PhysicalResource, reqType: PhysicalResourceType) => res.physical_type == reqType
    )

  /**
   * Allocates human resources for a task
   */
  private def allocateHumans(
    requiredTypes: List[PhysicalResourceType],
    availableHuman: List[HumanResource],
    usedHumanIds: Set[HumanResourceId]
  ): Result[List[HumanResourceId]] =
    allocateResources(
      requiredTypes,
      availableHuman,
      usedHumanIds,
      (res: HumanResource) => res.id,
      (res: HumanResource, reqType: PhysicalResourceType) => res.physicalResourceTypes.contains(reqType)
    )

  /**
   * Creates a TaskSchedule from task information and allocated resources
   */
  def createTaskSchedule(
    taskInfo: TaskInfo,
    startTime: Int,
    physicalIds: List[PhysicalResourceId],
    humanIds: List[HumanResourceId]
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
      humanIds
    )

  /**
   * Filters tasks that are eligible for execution at the current time
   */
  private def filterEligibleTasksForTime(
    candidateTasks: List[TaskInfo],
    state: SchedulingState,
    currentTime: Int
  ): List[TaskInfo] =
    candidateTasks.filter { task =>
      val progressKey = (task.orderId, task.productNumber)
      val completedTasks = state.productProgress.getOrElse(progressKey, 0)

      task.productTaskIndex.to == completedTasks &&
        task.earliestStart.to <= currentTime &&
        !isProductCurrentlyBeingProcessed(task.orderId, task.productNumber, state, currentTime)
    }

  /**
   * Checks if a product is currently being processed (has a task in execution)
   */
  private def isProductCurrentlyBeingProcessed(
    orderId: OrderId,
    productNumber: ProductNumber,
    state: SchedulingState,
    currentTime: Int
  ): Boolean =
    state.schedules.exists { schedule =>
      schedule.orderId == orderId &&
        schedule.productNumber == productNumber &&
        schedule.start.to <= currentTime &&
        schedule.end.to > currentTime
    }

  /**
   * Updates the ready tasks list by adding newly eligible tasks
   */
  private def updateReadyTasks(state: SchedulingState, allTasks: List[TaskInfo]): SchedulingState =
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

  /**
   * Updates the scheduling state after successfully scheduling a task
   */
  private def updateStateAfterScheduling(
    state: SchedulingState,
    scheduledTask: TaskInfo,
    schedule: TaskSchedule,
    currentTime: Int
  ): SchedulingState =
    val endTime = currentTime + scheduledTask.task.time.to
    val updatedAvailability = updateResourceAvailability(state.resourceAvailability, schedule, endTime)
    val progressKey = (scheduledTask.orderId, scheduledTask.productNumber)
    val currentProgress = state.productProgress.getOrElse(progressKey, 0)
    val updatedProgress = state.productProgress.updated(progressKey, currentProgress + 1)
    val remainingTasks = state.readyTasks.filterNot(isMatchingTask(_, scheduledTask))

    state.copy(
      readyTasks = remainingTasks,
      resourceAvailability = updatedAvailability,
      schedules = schedule :: state.schedules,
      productProgress = updatedProgress
    )

  /**
   * Updates resource availability map after scheduling a task
   */
  def updateResourceAvailability(
    availability: Map[String, Int],
    schedule: TaskSchedule,
    endTime: Int
  ): Map[String, Int] =
    val allUpdates = schedule.physicalResourceIds.map(id => {id.to} -> endTime) ++
      schedule.humanResourceIds.map(id => {id.to} -> endTime)
    availability ++ allUpdates

  /**
   * Creates XML output from schedule data
   */
  def create(xml: Elem): Result[Elem] =
    for {
      (orders, products, tasks, humanResources, physicalResources) <- Shared.scheduleDataRetriever(xml)
      schedules <- generateSchedule(orders, products, tasks, humanResources, physicalResources)
      outputXml = Shared.toXml(schedules, humanResources)
    } yield outputXml

  def create(xml: Elem, fileName: String): Result[Elem] =
    for {
      (orders, products, tasks, humanResources, physicalResources) <- Shared.scheduleDataRetriever(xml)
      schedules <- generateSchedule(orders, products, tasks, humanResources, physicalResources)
      outputXml = Shared.toXml(schedules, humanResources)
      _ = FileIO.save(fileName, outputXml)
    } yield outputXml