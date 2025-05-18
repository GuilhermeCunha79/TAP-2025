package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.Task
import pj.generators.SimpleTypeGenerator.{PhysicalResourceTypeGenerator, TaskIdGenerator, TaskTimeGenerator}

object TaskGenerator extends Properties("Task"):

  def generateTask: Gen[Task] =
    for {
      taskId <- TaskIdGenerator
      taskTime <- TaskTimeGenerator
      containerPhysicalResources <- Gen.nonEmptyListOf(PhysicalResourceTypeGenerator)
    } yield Task(taskId, taskTime, containerPhysicalResources)
    