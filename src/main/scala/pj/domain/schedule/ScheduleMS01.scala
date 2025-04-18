package pj.domain.schedule

import scala.xml.Elem
import pj.domain.*
import pj.domain.resources.*
import pj.domain.resources.Types.PhysicalResourceType
import pj.xml.{XML, XMLToDomain}



object ScheduleMS01 extends Schedule:

  def ScheduleDataRetriever(xml: Elem): Result[(List[PhysicalResourceType],List[Task],List[HumanResource],List[Product],List[Order])] =
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
      (physicalTypes,tasks,humanResources,products,orders)

  def FullLogic(physicalTypes: List[PhysicalResourceType],tasks: List[Task],humanResources: List[HumanResource],products: List[Product],orders: List[Order]): Elem = {
    
  }

  // TODO: Create the code to implement a functional domain model for schedule creation
  //       Use the xml.XML code to handle the xml elements
  //       Refer to https://github.com/scala/scala-xml/wiki/XML-Processing for xml creation
  def create(xml: Elem): Result[Elem] =
    val domain = for
      value <- ScheduleDataRetriever(xml)
    yield value

    domain match
      case Right(value) => Right(FullLogic(value._1,value._2,value._3,value._4,value._5))
      case Left(error) => Left(error)