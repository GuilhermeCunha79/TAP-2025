package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.TaskSchedule
import pj.generators.SimpleTypeGenerator.*
import pj.domain.resources.Types.*

object TaskScheduleGenerator extends Properties("TaskSchedule"):

  def generateTaskSchedule: Gen[TaskSchedule] =
    for {
      orderId <- OrderIdGenerator
      productNumber <- ProductNumberGenerator
      taskId <- TaskIdGenerator
      startRaw <- Gen.posNum[Int]
      duration <- Gen.chooseNum(1, 100)
      startTime <- TaskScheduleTime.from(startRaw) match
        case Right(st) => Gen.const(st)
        case Left(_) => Gen.fail
      endTime <- TaskScheduleTime.from(startRaw + duration) match
        case Right(et) => Gen.const(et)
        case Left(_) => Gen.fail
      physicalResources <- Gen.listOf(PhysicalResourceIdGenerator)
      humanNames <- Gen.listOf(HumanResourceNameGenerator)
    } yield TaskSchedule(
      orderId,
      productNumber,
      taskId,
      startTime,
      endTime,
      physicalResources,
      humanNames
    )