package pj.properties

import org.scalacheck.*
import pj.domain.resources.Types.*
import pj.domain.schedule.ScheduleMS01
import pj.generators.SimpleTypeGenerator.*
import pj.generators.TaskGenerator.generateTask
import pj.generators.TaskScheduleGenerator

object ScheduleProperties extends Properties("ScheduleProperties"):
  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(1000)


  property("generateSchedule produces a deterministic valid schedule") =
    Prop.forAll(TaskScheduleGenerator.generateDeterministicDomainData):
      case (orders, products, tasks, humanResources, physicalResources) =>
        val result = ScheduleMS01.generateSchedule(orders, products, tasks, humanResources, physicalResources)

        result match
          case Left(_) => Prop.falsified

          case Right(schedules) =>
            val groupedSchedules = schedules.groupBy(_.orderId)

            val allOrdersCorrect = orders.forall { order =>
              products.find(_.id == order.productId).exists { product =>
                val expectedTaskIds = product.tasksList
                val expectedTotalTasks = expectedTaskIds.size * order.quantity.to

                val scheduledForOrder = groupedSchedules.getOrElse(order.id, Nil)

                val correctCount = scheduledForOrder.sizeIs == expectedTotalTasks

                val groupedByProductInstance =
                  scheduledForOrder.groupBy(_.productNumber).values.toList

                val allInstancesHaveCorrectTasks =
                  groupedByProductInstance.forall { taskSchedules =>
                    val actualTaskIds = taskSchedules.map(_.taskId).toSet
                    actualTaskIds == expectedTaskIds.toSet
                  }

                correctCount && allInstancesHaveCorrectTasks
              }
            }

            Prop(allOrdersCorrect)


  property("generateSchedule produces a valid schedule") =
    Prop.forAll(TaskScheduleGenerator.generateDomainData):
      case (orders, products, tasks, humanResources, physicalResources) =>
        val result = ScheduleMS01.generateSchedule(orders, products, tasks, humanResources, physicalResources)

        result match
          case Left(error) =>
            Prop.falsified

          case Right(schedules) =>
            val allTasksScheduled = schedules.map(_.taskId).toSet.subsetOf(tasks.map(_.id).toSet)
            val allOrdersScheduled = schedules.map(_.orderId).toSet.subsetOf(orders.map(_.id).toSet)

            Prop(allTasksScheduled && allOrdersScheduled)


  property("no two tasks can use the same resource at the same time") =
    Prop.forAll(TaskScheduleGenerator.generateDeterministicDomainData):
      case (orders, products, tasks, humanResources, physicalResources) =>
        val result = ScheduleMS01.generateSchedule(orders, products, tasks, humanResources, physicalResources)

        result match
          case Left(_) =>
            Prop.falsified

          case Right(schedules) =>
            def overlaps(s1: Int, e1: Int, s2: Int, e2: Int): Boolean =
              s1 < e2 && s2 < e1

            val hasConflicts = schedules
              .combinations(2)
              .exists:
                case List(t1, t2) =>
                  overlaps(t1.start.to, t1.end.to, t2.start.to, t2.end.to)
                    &&
                    (
                      t1.physicalResourceIds.toSet.intersect(t2.physicalResourceIds.toSet).nonEmpty ||
                      t1.humanResourceNames.toSet.intersect(t2.humanResourceNames.toSet).nonEmpty
                    )
                case _ => false

            Prop(!hasConflicts)


  property("The order of human resources should not affect the allocation") =
    Prop.forAll(TaskScheduleGenerator.generateDomainData):
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


  property("The generated task schedules need to be unique") =
    Prop.forAll(TaskScheduleGenerator.generateDeterministicDomainData):
      case (orders, products, tasks, humanResources, physicalResources) =>
        val result = ScheduleMS01.generateSchedule(orders, products, tasks, humanResources, physicalResources)

        result match
          case Left(_) => Prop.undecided

          case Right(scheduleList) =>
            val seenKeys = scheduleList.map(s =>
              (s.orderId.to, s.productNumber.to, s.taskId.to, s.start.to, s.end.to)
            )
            Prop(seenKeys.distinct.sizeIs == seenKeys.sizeIs)


  property("Tasks are only scheduled when physical resources are available and properly allocated") =
    Prop.forAll(TaskScheduleGenerator.generateDomainData):
      case (orders, products, tasks, humanResources, physicalResources) =>
        val result = ScheduleMS01.generateSchedule(orders, products, tasks, humanResources, physicalResources)

        result match
          case Left(_) => Prop.undecided

          case Right(schedules) =>
            val allPhysicalValid = schedules.forall: schedule =>
              tasks.find(_.id == schedule.taskId) match
                case Some(task) =>
                  val physicalTypesAssigned = schedule.physicalResourceIds.flatMap(id =>
                    physicalResources.find(_.id == id).map(_.physical_type)
                  )
                  task.physicalResourceTypes.sizeIs == schedule.physicalResourceIds.size &&
                    task.physicalResourceTypes.forall(physicalTypesAssigned.contains)
                case None => false
            Prop(allPhysicalValid)


  property("Tasks are only scheduled when human resources are available and properly allocated") =
    Prop.forAll(TaskScheduleGenerator.generateDomainData):
      case (orders, products, tasks, humanResources, physicalResources) =>
        val result = ScheduleMS01.generateSchedule(orders, products, tasks, humanResources, physicalResources)

        result match
          case Left(_) => Prop.undecided

          case Right(schedules) =>
            val allHumanValid = schedules.forall: schedule =>
              tasks.find(_.id == schedule.taskId) match
                case Some(task) =>
                  schedule.humanResourceNames.forall: name =>
                    humanResources.find(_.name == name).exists: human =>
                      task.physicalResourceTypes.exists(human.physicalResourceTypes.contains)
                case None => false
            Prop(allHumanValid)


  property("product tasks are executed in order (linear production)") =
    Prop.forAll(TaskScheduleGenerator.generateDeterministicDomainData):
      case (orders, products, tasks, humanResources, physicalResources) =>
        val result = ScheduleMS01.generateSchedule(orders, products, tasks, humanResources, physicalResources)

        result match
          case Left(_) =>
            Prop.falsified

          case Right(schedules) =>
            val groupedByOrderAndProductInstance =
              schedules.groupBy(s => (s.orderId, s.productNumber)).view.mapValues(_.sortBy(_.start.to)).toMap

            val allLinear = groupedByOrderAndProductInstance.forall:
              case ((orderId, _), scheduledTasks) =>
                val maybeProduct = orders.find(_.id == orderId).flatMap(order =>
                  products.find(_.id == order.productId)
                )

                maybeProduct.exists { product =>
                  val taskOrder = product.tasksList
                  val scheduleMap = scheduledTasks.map(t => t.taskId -> t).toMap

                  taskOrder.sliding(2).forall:
                    case List(prevTaskId, nextTaskId) =>
                      (for {
                        prev <- scheduleMap.get(prevTaskId)
                        next <- scheduleMap.get(nextTaskId)
                      } yield prev.end.to <= next.start.to).getOrElse(false)

                    case _ => true
                }

            Prop(allLinear)



  property("generateTask assigns only available types") =
    Prop.forAll(Gen.nonEmptyListOf(PhysicalResourceTypeGenerator))(
      types => Prop.forAll(generateTask(types))(
        task => task.physicalResourceTypes.forall(types.contains)
      )
    )

  // *** SIMPLE TYPES ***

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

  property("OrderQuantityGenerator generates valid OrderQuantities in range") =
    Prop.forAll(OrderQuantityGenerator)(
      q => {
        val asInt = q.to
        (1 to 5).contains(asInt) && OrderQuantity.from(q.toString).isRight
      }
    )

  property("Order references only existing products") =
    Prop.forAll(TaskScheduleGenerator.generateDeterministicDomainData):
      case (orders, products, _, _, _) =>
        orders.forall(o => products.exists(_.id == o.productId))

  property("TaskIdGenerator generates valid Task IDs with prefix") =
    Prop.forAll(TaskIdGenerator) { id =>
      id.to.startsWith("TSK_") && TaskId.from(id.to).isRight
    }

  property("TaskTimeGenerator generates valid positive TaskTimes") =
    Prop.forAll(TaskTimeGenerator)(
      time => time.to.toString.forall(_.isDigit) && time.to > 0 && TaskTime.from(time.to.toString).isRight
    )

  property("PhysicalResourceIdGenerator generates valid Physical Resource IDs with prefix") =
    Prop.forAll(PhysicalResourceIdGenerator):
      id => id.to.startsWith("PRS_") && PhysicalResourceId.from(id.to).isRight

  property("PhysicalResourceIdGenerator generates valid IDs with prefix") =
    Prop.forAll(PhysicalResourceIdGenerator)(
      id => id.to.startsWith("PRS_") && PhysicalResourceId.from(id.to).isRight
    )
    
  property("PhysicalResourceTypeGenerator generates non-empty types") =
      Prop.forAll(PhysicalResourceTypeGenerator)(
        t => t.to.nonEmpty && PhysicalResourceType.from(t.to).isRight
      )

  property("HumanResourceIdGenerator generates valid Human Resource IDs with prefix") =
    Prop.forAll(HumanResourceIdGenerator) { id =>
      id.to.startsWith("HRS_") && HumanResourceId.from(id.to).isRight
    }

  property("HumanResourceNameGenerator generates non-empty names") =
    Prop.forAll(HumanResourceNameGenerator)(
      name => name.to.nonEmpty && HumanResourceName.from(name.to).isRight
    )

  property("Generated OrderId, ProductId and TaskId are distinct") =
    Prop.forAll(OrderIdGenerator, ProductIdGenerator, TaskIdGenerator)(
      (oId, pId, tId) => oId.to != pId.to && oId.to != tId.to && pId.to != tId.to
    )

