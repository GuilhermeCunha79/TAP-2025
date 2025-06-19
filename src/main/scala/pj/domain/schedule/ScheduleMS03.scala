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
      initialState = createInitialState(allProductInstances, physicalResources, humanResources)
      result <- scheduleAllTasks(initialState, allProductInstances, physicalResources, humanResources)

    } yield result.schedules.reverse

  private def createInitialState(
    allProductInstances: List[TaskInfo],
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): SchedulingState =
    val initialReadyTasks = allProductInstances.filter(_.productTaskIndex.to == 0)
    val initialResourceAvailability = (
      physicalResources.map(r => s"PHYS_${r.id.to}" -> 0) ++
        humanResources.map(r => s"HUMAN_${r.id.to}" -> 0)
      ).toMap

    SchedulingState(
      readyTasks = initialReadyTasks,
      resourceAvailability = initialResourceAvailability,
      schedules = List.empty,
      productProgress = Map.empty
    )

  private def validateResourceRequirements(
    tasks: List[Task],
    physicalResources: List[PhysicalResource],
    humanResources: List[HumanResource]
  ): Result[Unit] =
    val physicalResourceCounts = physicalResources.groupBy(_.physical_type).view.mapValues(_.size).toMap
    val humanResourceTypes = humanResources.flatMap(_.physicalResourceTypes).toSet

    tasks.foldLeft[Result[Unit]](Right(())) { (acc, task) =>
      acc.flatMap(_ => validateTask(task, physicalResourceCounts, humanResourceTypes))
    }

  private def validateTask(
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

  private def createAllProductInstances(
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

  private def scheduleAllTasks(
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

  private def advanceTimeAndRetry(
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

  private def scheduleNextBatch(
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

  private def prioritizeTasks(tasks: List[TaskInfo]): List[TaskInfo] =
    val rarityMap = tasks
      .flatMap(_.task.physicalResourceTypes)
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toMap

    tasks.sortBy(task =>
      (-task.task.time.to, task.task.physicalResourceTypes.map(rarityMap.getOrElse(_, 0)).sum)
    )
  
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

  private def getAvailableResourcesAtTime(
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

  private def scheduleBatchRecursively(
    state: SchedulingState,
    tasks: List[TaskInfo],
    currentTime: Int,
    availablePhysical: List[PhysicalResource],
    availableHuman: List[HumanResource],
    usedPhysicalIds: Set[PhysicalResourceId],
    usedHumanNames: Set[HumanResourceId]
  ): Result[SchedulingState] =
    tasks match
      case Nil => Right(state)
      case taskInfo :: remainingTasks =>
        tryScheduleTask(taskInfo, currentTime, availablePhysical, availableHuman, usedPhysicalIds, usedHumanNames)
          .fold(
            _ => scheduleBatchRecursively(
              state, remainingTasks, currentTime, availablePhysical, availableHuman,
              usedPhysicalIds, usedHumanNames
            ),
            { case (physicalIds, humanNames) =>
              createTaskSchedule(taskInfo, currentTime, physicalIds, humanNames).flatMap { schedule =>
                val newState = updateStateAfterScheduling(state, taskInfo, schedule, currentTime)
                val updatedUsedPhysical = usedPhysicalIds ++ physicalIds.toSet
                val updatedUsedHuman = usedHumanNames ++ humanNames.toSet
                val filteredRemaining = remainingTasks.filterNot(isMatchingTask(_, taskInfo))

                scheduleBatchRecursively(
                  newState, filteredRemaining, currentTime, availablePhysical, availableHuman,
                  updatedUsedPhysical, updatedUsedHuman
                )
              }
            }
          )

  private def tryScheduleTask(
   taskInfo: TaskInfo,
   currentTime: Int,
   availablePhysical: List[PhysicalResource],
   availableHuman: List[HumanResource],
   usedPhysicalIds: Set[PhysicalResourceId],
   usedHumanNames: Set[HumanResourceId]
 ): Result[(List[PhysicalResourceId], List[HumanResourceId])] =
    for {
      physicalIds <- allocateResources(taskInfo.task.physicalResourceTypes, availablePhysical, usedPhysicalIds)
      humanNames <- allocateHumans(taskInfo.task.physicalResourceTypes, availableHuman, usedHumanNames)
    } yield (physicalIds, humanNames)

  private def isMatchingTask(t1: TaskInfo, t2: TaskInfo): Boolean =
    t1.orderId == t2.orderId && t1.productNumber == t2.productNumber && t1.taskId == t2.taskId

  private def allocateResources[T, R](
     requiredTypes: List[PhysicalResourceType],
     availableResources: List[T],
     usedIds: Set[R],
     extractId: T => R,
     matchesType: (T, PhysicalResourceType) => Boolean
   ): Result[List[R]] =
    def allocateRecursively(
       remaining: List[PhysicalResourceType],
       allocated: List[R],
       used: Set[R]
     ): Result[List[R]] =
      val sortedRemaining = remaining.sortBy { t =>
        availableResources.count(res =>
          matchesType(res, t) && !used.contains(extractId(res))
        )
      }
      sortedRemaining match
        case Nil => Right(allocated.reverse)
        case requiredType :: rest =>
          availableResources.find { res =>
            matchesType(res, requiredType) && !used.contains(extractId(res))
          }.fold[Result[List[R]]](
            Left(DomainError.ImpossibleSchedule)
          ) { resource =>
            val id = extractId(resource)
            allocateRecursively(rest, id :: allocated, used + id)
          }

    allocateRecursively(requiredTypes, List.empty, usedIds)

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

  private def allocateHumans(
    requiredTypes: List[PhysicalResourceType],
    availableHuman: List[HumanResource],
    usedNames: Set[HumanResourceId]
  ): Result[List[HumanResourceId]] =
    allocateResources(
      requiredTypes,
      availableHuman,
      usedNames,
      (res: HumanResource) => res.id,
      (res: HumanResource, reqType: PhysicalResourceType) => res.physicalResourceTypes.contains(reqType)
    )

  private def createTaskSchedule(
    taskInfo: TaskInfo,
    startTime: Int,
    physicalIds: List[PhysicalResourceId],
    humanNames: List[HumanResourceId]
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

  private def updateResourceAvailability(
    availability: Map[String, Int],
    schedule: TaskSchedule,
    endTime: Int
  ): Map[String, Int] =
    val allUpdates = schedule.physicalResourceIds.map(id => s"PHYS_${id.to}" -> endTime) ++
      schedule.humanResourceIds.map(name => s"HUMAN_${name.to}" -> endTime)
    availability ++ allUpdates

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

  private def getHumanNameById(
    humanId: HumanResourceId,
    humanResources: List[HumanResource]
  ): String =
    humanResources.find(_.id == humanId).map(_.name.to).getOrElse(humanId.to)

  def toXml(schedules: List[TaskSchedule], humanResources: List[HumanResource]): Elem =
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
          {sched.humanResourceIds.map(humanId =>
            <Human name={getHumanNameById(humanId, humanResources)}/>
        )}
        </HumanResources>
      </TaskSchedule>
    }}
    </Schedule>

  def create(xml: Elem): Result[Elem] =
    for {
      (orders, products, tasks, humanResources, physicalResources) <- scheduleDataRetriever(xml)
      schedules <- generateSchedule(orders, products, tasks, humanResources, physicalResources)
      outputXml = toXml(schedules, humanResources)
      _ = FileIO.save("output.xml", outputXml)
    } yield outputXml

  def create(xml: Elem, fileName: String): Result[Elem] =
    for {
      (orders, products, tasks, humanResources, physicalResources) <- scheduleDataRetriever(xml)
      schedules <- generateSchedule(orders, products, tasks, humanResources, physicalResources)
      outputXml = toXml(schedules, humanResources)
      _ = FileIO.save(fileName, outputXml)
    } yield outputXml