package pj.properties

import org.scalacheck.*
import pj.domain.resources.TaskSchedule
import pj.domain.schedule.ScheduleMS01
import pj.generators.TaskScheduleGenerator

object ScheduleProperties extends Properties("ScheduleProperties"):
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(100)

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

  property("The order of human resources should not affect the allocation") = Prop.forAll(TaskScheduleGenerator.generateDomainData):
    case (orders, products, tasks, humanResources, physicalResources) =>
      humanResources match
        case original :: second :: rest =>
          val reversed = (original :: second :: rest).reverse

          val resultOriginal = ScheduleMS01.generateSchedule(orders, products, tasks, original :: second :: rest, physicalResources)
          val resultReversed = ScheduleMS01.generateSchedule(orders, products, tasks, reversed, physicalResources)

          (resultOriginal, resultReversed) match
            case (Right(scheduleOriginal), Right(scheduleReversed)) =>
              val allocOriginal = scheduleOriginal.flatMap(_.humanResourceNames).map(_.to).toSet
              val allocReversed = scheduleReversed.flatMap(_.humanResourceNames).map(_.to).toSet
              Prop(allocOriginal == allocReversed)

            case (Left(_), Left(_)) =>
              Prop.passed

            case _ =>
              Prop.falsified

        case _ =>
          Prop.undecided

