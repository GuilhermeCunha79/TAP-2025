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
      requiredTypes <- Gen.someOf(availableTypes).map(_.toList).suchThat(_.nonEmpty)
    } yield Task(taskId, taskTime, requiredTypes)