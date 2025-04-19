package pj.domain.schedule

import pj.domain.*
import pj.domain.resources.*
import pj.domain.resources.Types.*
import pj.xml.{XML, XMLToDomain}

import scala.xml.Elem

object ScheduleMS01 extends Schedule {

  private def scheduleDataRetriever(xml: Elem): Result[(List[PhysicalResource], List[PhysicalResourceType], List[Task], List[HumanResource], List[Product], List[Order])] =
    for
      physicalNode <- XML.fromNode(xml, "PhysicalResources")
      physicalResources <- XML.traverse(physicalNode \ "Physical", XMLToDomain.getPhysicalResource)
      physicalTypes = physicalResources.map(_.name).distinct

      tasksNode <- XML.fromNode(xml, "Tasks")
      tasks <- XML.traverse(tasksNode \ "Task", XMLToDomain.getTask(physicalTypes))

      humanNode <- XML.fromNode(xml, "HumanResources")
      humanResources <- XML.traverse(humanNode \ "Human", XMLToDomain.getHumanResource(physicalTypes))

      productsNode <- XML.fromNode(xml, "Products")
      products <- XML.traverse(productsNode \ "Product", XMLToDomain.getProduct(tasks))

      ordersNode <- XML.fromNode(xml, "Orders")
      orders <- XML.traverse(ordersNode \ "Order", XMLToDomain.getOrder(products))
    yield
      (physicalResources, physicalTypes, tasks, humanResources, products, orders)

  private def allocatePhysical(taskId: TaskId, types: List[PhysicalResourceType], physicalResources: List[PhysicalResource]): Result[List[PhysicalResourceId]] =
    types.foldLeft[Result[(List[PhysicalResourceId], Set[PhysicalResourceId])]](Right((Nil, Set.empty))) {
      case (acc, typ) => acc.flatMap { case (assigned, used) =>
        physicalResources
          .find(pr => pr.name.equal(typ) && !used.contains(pr.id))
          .map(_.id)
          .toRight(DomainError.ResourceUnavailable(taskId.to, typ.to))
          .map(id => (id :: assigned, used + id))
      }
    }.map(_._1.reverse)

  private def allocateHuman(taskId: TaskId, types: List[PhysicalResourceType], humanResources: List[HumanResource]): Result[List[String]] =
    types.foldLeft[Result[(List[String], Set[String])]](Right((Nil, Set.empty))) {
      case (acc, typ) => acc.flatMap { case (assigned, used) =>
        humanResources
          .find(hr => hr.physicalResources.contains(typ) && !used.contains(hr.name))
          .toRight(DomainError.ResourceUnavailable(taskId.to, typ.to))
          .map(hr => (hr.name :: assigned, used + hr.name))
      }
    }.map(_._1.reverse)

  private def generateSchedule(
    physicalResources: List[PhysicalResource],
    physicalTypes: List[PhysicalResourceType],
    tasks: List[Task],
    humanResources: List[HumanResource],
    products: List[Product],
    orders: List[Order]
  ): Result[Elem] =

    type Acc = (List[TaskSchedule], Int)
    val initial: Result[Acc] = Right((Nil, 0))

    val finalResult = orders.foldLeft(initial) { (ordAcc, order) =>
      ordAcc.flatMap { case (scheds, currentTime) =>
        val productOpt = products.find(_.id == order.productId)
          .toRight(DomainError.ProductDoesNotExist(order.productId.to))

        (1 to order.quantity.to).foldLeft[Result[Acc]](Right((scheds, currentTime))):
          case (prodAcc, prodNum) => prodAcc.flatMap { case (ps, time0) =>
            productOpt.flatMap { p =>
              p.tasksList.foldLeft[Result[Acc]](Right((ps, time0))):
                case (taskAcc, tId) => taskAcc.flatMap { case (ts, tStart) =>
                  tasks.find(_.id == tId)
                    .toRight(DomainError.TaskDoesNotExist(tId.to))
                    .flatMap { t =>
                      for {
                        physIds <- allocatePhysical(t.id, t.physicalResources, physicalResources)
                        humIds <- allocateHuman(t.id, t.physicalResources, humanResources)
                      } yield
                        val end = tStart + t.time.to
                        val schedule = TaskSchedule(
                          order.id, prodNum, t.id,
                          tStart, end,
                          physIds, humIds
                        )
                        (schedule :: ts, end)
                    }
                }
            }
          }
      }
    }

    finalResult.map { case (allSchedules, _) =>
      toXml(allSchedules)
    }

  // Function to generate the XML
  private def toXml(schedules: List[TaskSchedule]): Elem =
    <Schedule xmlns="http://www.dei.isep.ipp.pt/tap-2025"
              xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
              xsi:schemaLocation="http://www.dei.isep.ipp.pt/tap-2025 ../../schedule.xsd ">
      {schedules.sortBy(s => (s.orderId.to, s.productNumber, s.start)).map { sched =>
      <TaskSchedule order={sched.orderId.to}
                    productNumber={sched.productNumber.toString}
                    task={sched.taskId.to}
                    start={sched.start.toString}
                    end={sched.end.toString}>
        <PhysicalResources>
          {sched.physicalResourceIds.map(id =>
            <Physical id={id.to}/>
        )}
        </PhysicalResources>
        <HumanResources>
          {sched.humanResourceNames.map(name =>
            <Human name={name}/>
        )}
        </HumanResources>
      </TaskSchedule>
    }}
    </Schedule>

  // The main function to create the schedule
  def create(xml: Elem): Result[Elem] =
    scheduleDataRetriever(xml) match
      case Right((physResources, physTypes, tasks, humanResources, products, orders)) => 
        generateSchedule(physResources, physTypes, tasks, humanResources, products, orders)
      case Left(error) => 
        Left(error)
}