object heuristics:

private def prioritizeTasks(tasks: List[TaskInfo],
                            heuristic: String,
                            currentTime: Int): List[TaskInfo] =
  prioritizeTasks(tasks, heuristic, currentTime, List.empty)

private def prioritizeTasks(tasks: List[TaskInfo],
                            heuristic: String,
                            currentTime: Int,
                            physicalResources: List[PhysicalResource]): List[TaskInfo] =
  heuristic match
    case "SJF_RARITY" => prioritizeTasksSJF(tasks)
    case "RARITY_FIRST" => prioritizeTasksRarityFirst(tasks)
    case "WEIGHTED" => prioritizeTasksWeighted(tasks)
    case "CRITICAL_RATIO" => prioritizeTasksCriticalRatio(tasks, currentTime)
    case "RESOURCE_EFFICIENCY" => prioritizeTasksResourceEfficiency(tasks)
    case "FIFO" => prioritizeTasksFIFO(tasks)
    case "BALANCED" => prioritizeTasksBalanced(tasks)
    case "LEAST_TYPES" => prioritizeTasksLeastResourceTypes(tasks)
    case "MOST_CONSTRAINED" => prioritizeTasksMostConstrained(tasks, physicalResources)
    case "SLACK_BASED" => prioritizeTasksSlackBased(tasks, currentTime)
    case _ => // Default: LJF_RARITY (Longest Job First + Resource Rarity)
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
  val prioritizedTasks = prioritizeTasks(eligibleTasks, "BALANCED", currentTime, physicalResources)
  val (availablePhysical, availableHuman) = getAvailableResourcesAtTime(
    physicalResources, humanResources, state.resourceAvailability, currentTime
  )

  scheduleBatchRecursively(state, prioritizedTasks, currentTime, availablePhysical, availableHuman, Set.empty, Set.empty)

/**
 * 1: Shortest Job First (SJF) + Resource Rarity
 */
private def prioritizeTasksSJF(tasks: List[TaskInfo]): List[TaskInfo] =
  val rarityMap = tasks
    .flatMap(_.task.physicalResourceTypes)
    .groupBy(identity)
    .view
    .mapValues(_.size)
    .toMap

  tasks.sortBy(task =>
    (task.task.time.to, task.task.physicalResourceTypes.map(rarityMap.getOrElse(_, 0)).sum)
  )

/**
 * 2: Resource Rarity First + Longest Job First
 */
private def prioritizeTasksRarityFirst(tasks: List[TaskInfo]): List[TaskInfo] =
  val rarityMap = tasks
    .flatMap(_.task.physicalResourceTypes)
    .groupBy(identity)
    .view
    .mapValues(_.size)
    .toMap

  tasks.sortBy(task =>
    (task.task.physicalResourceTypes.map(rarityMap.getOrElse(_, 0)).sum, -task.task.time.to)
  )

/**
 * 3: Resource Count + Duration Weighted
 */
private def prioritizeTasksWeighted(tasks: List[TaskInfo]): List[TaskInfo] =
  val rarityMap = tasks
    .flatMap(_.task.physicalResourceTypes)
    .groupBy(identity)
    .view
    .mapValues(_.size)
    .toMap

  tasks.sortBy(task =>
    (
      -task.task.time.to * 2, // Weight 2 for duration
      task.task.physicalResourceTypes.size, // Number of needed resources
      task.task.physicalResourceTypes.map(rarityMap.getOrElse(_, 0)).sum
    )
  )

/**
 * 4: Critical Ratio (CR)
 */
private def prioritizeTasksCriticalRatio(tasks: List[TaskInfo], currentTime: Int): List[TaskInfo] =
  val rarityMap = tasks
    .flatMap(_.task.physicalResourceTypes)
    .groupBy(identity)
    .view
    .mapValues(_.size)
    .toMap

  tasks.sortBy(task =>
    (
      // Critical ratio approximation (lower is more critical)
      task.task.time.to.toDouble / math.max(1, task.earliestStart.to - currentTime + task.task.time.to),
      task.task.physicalResourceTypes.map(rarityMap.getOrElse(_, 0)).sum
    )
  )

/**
 * 5: Resource Efficiency (Resources per Time Unit)
 */
private def prioritizeTasksResourceEfficiency(tasks: List[TaskInfo]): List[TaskInfo] =
  tasks.sortBy(task =>
    (
      task.task.physicalResourceTypes.size.toDouble / math.max(1, task.task.time.to), // Resources per time
      -task.task.time.to // Tiebreaker: longer tasks first
    )
  )

/**
 * 6: FIFO com Resource Rarity
 */
private def prioritizeTasksFIFO(tasks: List[TaskInfo]): List[TaskInfo] =
  val rarityMap = tasks
    .flatMap(_.task.physicalResourceTypes)
    .groupBy(identity)
    .view
    .mapValues(_.size)
    .toMap

  tasks.sortBy(task =>
    (
      task.orderId.to,
      task.productNumber.to,
      task.productTaskIndex.to,
      task.task.physicalResourceTypes.map(rarityMap.getOrElse(_, 0)).sum
    )
  )

/**
 * 7: Balanced Approach
 */
private def prioritizeTasksBalanced(tasks: List[TaskInfo]): List[TaskInfo] =
  val rarityMap = tasks
    .flatMap(_.task.physicalResourceTypes)
    .groupBy(identity)
    .view
    .mapValues(_.size)
    .toMap

  val maxTime = tasks.map(_.task.time.to).maxOption.getOrElse(1)
  val maxRarity = rarityMap.values.maxOption.getOrElse(1)

  tasks.sortBy(task =>
    (
      // Normalized weighted sum
      (task.task.time.to.toDouble / maxTime) * 0.4 + // 40% weight on duration
        (task.task.physicalResourceTypes.map(rarityMap.getOrElse(_, 0)).sum.toDouble / maxRarity) * 0.4 + // 40% weight on rarity
        (task.orderId.to.hashCode.abs % 100).toDouble * 0.2 // 20% weight on order variation
      )
  )