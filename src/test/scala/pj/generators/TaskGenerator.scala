package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.*
import pj.domain.resources.Types.PhysicalResourceType
import pj.generators.SimpleTypeGenerator.{TaskIdGenerator, TaskTimeGenerator}

object TaskGenerator extends Properties("Task"):

  def generateTask(availableTypes: List[PhysicalResourceType]): Gen[Task] =
    for {
      taskId <- TaskIdGenerator
      taskTime <- TaskTimeGenerator
      requiredTypes <- Gen.someOf(availableTypes).suchThat(_.nonEmpty).map(_.toList)
    } yield Task(taskId, taskTime, requiredTypes)
  

  def generateTaskList: List[PhysicalResourceType] => Gen[List[Task]] =
    (types) => Gen.nonEmptyListOf(generateTask(types))

  def generateDeterministicTask(humanResources: List[HumanResource], physicalResources: List[PhysicalResource]): Gen[Task] =
    val availableTypes: Set[PhysicalResourceType] = physicalResources.map(_.physical_type).toSet

    val typeToHuman: Map[PhysicalResourceType, List[HumanResource]] =
      availableTypes.map(t => t -> humanResources.filter(_.physicalResourceTypes.contains(t))).toMap

    val assignableTypes: List[PhysicalResourceType] =
      typeToHuman.filter(_._2.nonEmpty).keys.toList

    val maxAssignable = humanResources.length.min(assignableTypes.length)

    for {
      n <- Gen.choose(1, maxAssignable)
      selectedTypes <- Gen.pick(n, assignableTypes)
      id <- TaskIdGenerator
      time <- TaskTimeGenerator
    } yield Task(id, time, selectedTypes.toList)


  def generateDeterministicTaskList(humanResources: List[HumanResource], physicalResources: List[PhysicalResource]): Gen[List[Task]] =
    Gen.nonEmptyListOf(generateDeterministicTask(humanResources, physicalResources))