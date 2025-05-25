package pj.properties

import org.scalacheck.*
import pj.domain.resources.TaskSchedule
import pj.domain.schedule.ScheduleMS01
import pj.generators.TaskScheduleGenerator

object ScheduleProperties extends Properties("ScheduleProperties"):
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(1000)
/*

  property("generateSchedule produces a deterministic valid schedule") = Prop.forAll(TaskScheduleGenerator.generateDeterministicDomainData):
    case (orders, products, tasks, humanResources, physicalResources) =>
      val result = ScheduleMS01.generateSchedule(orders, products, tasks, humanResources, physicalResources)

      result match
        case Left(error) =>
          Prop.falsified

        case Right(schedules) =>
          val allTasksScheduled = schedules.map(_.taskId).toSet.subsetOf(tasks.map(_.id).toSet)
          val allOrdersScheduled = schedules.map(_.orderId).toSet.subsetOf(orders.map(_.id).toSet)

          Prop(allTasksScheduled && allOrdersScheduled)


  property("generateSchedule produces a valid schedule") = Prop.forAll(TaskScheduleGenerator.generateDomainData):
    case (orders, products, tasks, humanResources, physicalResources) =>
      val result = ScheduleMS01.generateSchedule(orders, products, tasks, humanResources, physicalResources)

      result match
        case Left(error) =>
          Prop.falsified

        case Right(schedules) =>
          val allTasksScheduled = schedules.map(_.taskId).toSet.subsetOf(tasks.map(_.id).toSet)
          val allOrdersScheduled = schedules.map(_.orderId).toSet.subsetOf(orders.map(_.id).toSet)

          Prop(allTasksScheduled && allOrdersScheduled)
*/

  /*property("The order of human resources should not affect the allocation") = Prop.forAll(TaskScheduleGenerator.generateDomainData):
    case (orders, products, tasks, humanResources, physicalResources) =>
      humanResources match
        case original :: second :: rest =>
          val shuffled = scala.util.Random.shuffle(original :: second :: rest)

          val resultOriginal = ScheduleMS01.generateSchedule(orders, products, tasks, original :: second :: rest, physicalResources)
          val resultShuffled = ScheduleMS01.generateSchedule(orders, products, tasks, shuffled, physicalResources)

          (resultOriginal, resultShuffled) match
            case (Right(scheduleOriginal), Right(scheduleShuffled)) =>
              val allocOriginal = scheduleOriginal.flatMap(_.humanResourceNames).map(_.to).toSet
              val allocShuffled = scheduleShuffled.flatMap(_.humanResourceNames).map(_.to).toSet
              Prop(allocOriginal == allocShuffled)

            case (Left(_), Left(_)) =>
              Prop.passed

            case _ =>
              Prop.falsified

        case _ =>
          Prop.undecided
    */

  property("The generated task schedules need to be unique") = Prop.forAll(TaskScheduleGenerator.generateDeterministicDomainData):
    case (orders, products, tasks, humanResources, physicalResources) =>
      ScheduleMS01.generateSchedule(orders, products, tasks, humanResources, physicalResources) match
        case Left(_) => Prop.falsified
        case Right(scheduleList) =>
          val seenKeys = scheduleList.map(s =>
            (s.orderId.to, s.productNumber.to, s.taskId.to, s.start.to, s.end.to)
          )
          Prop(seenKeys.distinct.sizeIs == seenKeys.sizeIs)

