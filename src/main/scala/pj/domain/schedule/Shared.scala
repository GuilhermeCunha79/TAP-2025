package pj.domain.schedule

import pj.domain.*
import pj.domain.resources.*
import pj.domain.resources.Types.*
import pj.xml.{XML, XMLToDomain}

import scala.xml.Elem

object Shared:
  def scheduleDataRetriever(xml: Elem): Result[(
    List[Order],
    List[Product],
    List[Task],
    List[HumanResource],
    List[PhysicalResource]
    )] =
    for {
      physicalNode <- XML.fromNode(xml, "PhysicalResources")
      physicalResources <- XML.traverse(physicalNode \ "Physical", XMLToDomain.getPhysicalResource)
      physicalTypes = physicalResources.map(_.physical_type).distinct

      tasksNode <- XML.fromNode(xml, "Tasks")
      tasks <- XML.traverse(tasksNode \ "Task", XMLToDomain.getTask(physicalTypes))

      humanNode <- XML.fromNode(xml, "HumanResources")
      humanResources <- XML.traverse(humanNode \ "Human", XMLToDomain.getHumanResource(physicalTypes))

      productsNode <- XML.fromNode(xml, "Products")
      products <- XML.traverse(productsNode \ "Product", XMLToDomain.getProduct(tasks))

      ordersNode <- XML.fromNode(xml, "Orders")
      orders <- XML.traverse(ordersNode \ "Order", XMLToDomain.getOrder(products))
    } yield (orders, products, tasks, humanResources, physicalResources)

  private def getHumanNameById(
      humanId: HumanResourceId,
      humanResources: List[HumanResource]
    ): String =
    humanResources.find(_.id == humanId).map(_.name.to).getOrElse(humanId.to)

  def toXml(schedules: List[TaskSchedule], humanResources: List[HumanResource]): Elem =
    <Schedule xmlns="http://www.dei.isep.ipp.pt/tap-2025"
              xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
              xsi:schemaLocation="http://www.dei.isep.ipp.pt/tap-2025 ../../schedule.xsd ">
      {schedules.sortBy(_.start.to).map { sched =>
      <TaskSchedule order={sched.orderId.to}
                    productNumber={sched.productNumber.to.toString}
                    task={sched.taskId.to}
                    start={sched.start.to.toString}
                    end={sched.end.to.toString}>
        <PhysicalResources>
          {sched.physicalResourceIds.map(id =>
            <Physical id={id.to}/>
        )}
        </PhysicalResources>
        <HumanResources>
          {sched.humanResourceIds.map(humanId =>
            <Human name={getHumanNameById(humanId, humanResources)}/>
        )}
        </HumanResources>
      </TaskSchedule>
    }}
    </Schedule>
