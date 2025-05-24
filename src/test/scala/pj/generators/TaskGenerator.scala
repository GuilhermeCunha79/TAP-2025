package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.Task
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