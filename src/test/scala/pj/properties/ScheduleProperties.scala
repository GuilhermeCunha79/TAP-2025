package pj.properties

import org.scalacheck.*
import pj.domain.resources.{HumanResource, PhysicalResource, TaskSchedule}
import pj.domain.resources.Types.{HumanResourceName, OrderId, PhysicalResourceId, ProductId, ProductName, ProductNumber, TaskTime}
import pj.domain.schedule.ScheduleMS01
import pj.generators.SimpleTypeGenerator.{HumanResourceIdGenerator, HumanResourceNameGenerator, OrderIdGenerator, OrderQuantityGenerator, PhysicalResourceIdGenerator, PhysicalResourceTypeGenerator, ProductIdGenerator, ProductNameGenerator, ProductNumberGenerator, TaskIdGenerator, TaskTimeGenerator}
import pj.generators.TaskGenerator.{generateDeterministicTask, generateDeterministicTaskList, generateTask}
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
/*
  property("The generated task schedules need to be unique") = Prop.forAll(TaskScheduleGenerator.generateDeterministicDomainData):
    case (orders, products, tasks, humanResources, physicalResources) =>
      ScheduleMS01.generateSchedule(orders, products, tasks, humanResources, physicalResources) match
        case Left(_) => Prop.falsified
        case Right(scheduleList) =>
          val seenKeys = scheduleList.map(s =>
            (s.orderId.to, s.productNumber.to, s.taskId.to, s.start.to, s.end.to)
          )
          Prop(seenKeys.distinct.sizeIs == seenKeys.sizeIs)

*/

  property("generateTask assigns only available types") =
    Prop.forAll(Gen.nonEmptyListOf(PhysicalResourceTypeGenerator))(
      types => Prop.forAll(generateTask(types))(
        task => task.physicalResourceTypes.forall(types.contains)
      )
    )


  property("ProductIdGenerator generates valid ProductIds with correct prefix") =
    Prop.forAll(ProductIdGenerator)(
      id => id.to.startsWith("PRD_") && ProductId.from(id.to).isRight
    )

  property("ProductNameGenerator generates non-empty valid ProductNames") =
    Prop.forAll(ProductNameGenerator)(
      name => name.to.nonEmpty && ProductName.from(name.to).isRight
    )

  property("ProductNumberGenerator generates positive valid ProductNumbers") =
    Prop.forAll(ProductNumberGenerator)(
      num => num.to > 0 && ProductNumber.from(num.to).isRight
    )

  property("OrderIdGenerator generates valid OrderIds with correct prefix") =
    Prop.forAll(OrderIdGenerator)(
      id => id.to.startsWith("ORD_") && OrderId.from(id.to).isRight
    )

  //TODO: Alterar valor (1,10)
  /*property("OrderQuantityGenerator generates valid OrderQuantities in range") =
    Prop.forAll(OrderQuantityGenerator)(
      q => {
        val asInt = q.value.toInt
        (1 to 2).contains(asInt) && OrderQuantity.from(q.value).isRight
      }
    )*/

  property("TaskTimeGenerator generates valid positive TaskTimes") =
    Prop.forAll(TaskTimeGenerator)(
      time => time.to.toString.forall(_.isDigit) && time.to > 0 && TaskTime.from(time.to.toString).isRight
    )

  property("PhysicalResourceIdGenerator generates valid IDs with prefix") =
    Prop.forAll(PhysicalResourceIdGenerator)(
      id => id.to.startsWith("PRS_") && PhysicalResourceId.from(id.to).isRight
    )

  property("HumanResourceNameGenerator generates non-empty names") =
    Prop.forAll(HumanResourceNameGenerator)(
      name => name.to.nonEmpty && HumanResourceName.from(name.to).isRight
    )

  property("Generated OrderId, ProductId and TaskId are distinct") =
    Prop.forAll(OrderIdGenerator, ProductIdGenerator, TaskIdGenerator)(
      (oId, pId, tId) => oId.to != pId.to && oId.to != tId.to && pId.to != tId.to
    )

