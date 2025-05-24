package pj.properties

import org.scalacheck.*
import pj.domain.schedule.ScheduleMS01
import pj.generators.TaskScheduleGenerator

object ScheduleProperties extends Properties("ScheduleProperties"):
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(1000)


  property("generateSchedule produces a valid schedule") = Prop.forAll(TaskScheduleGenerator.generateDomainData):
    case (orders, products, tasks, humanResources, physicalResources) =>
      val result = ScheduleMS01.generateSchedule(orders, products, tasks, humanResources, physicalResources)

      result match
        case Left(error) =>
          println("Schedule failed with error:")
          println(error)
          println("Orders:")
          orders.foreach(println)
          println("Products:")
          products.foreach(println)
          println("Tasks:")
          tasks.foreach(println)
          println("HumanResources:")
          humanResources.foreach(println)
          println("PhysicalResources:")
          physicalResources.foreach(println)
          false

        case Right(schedules) =>
          val allTasksScheduled = schedules.map(_.taskId).toSet.subsetOf(tasks.map(_.id).toSet)
          val allOrdersScheduled = schedules.map(_.orderId).toSet.subsetOf(orders.map(_.id).toSet)

          allTasksScheduled && allOrdersScheduled
