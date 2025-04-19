package pj.domain.resources

import pj.domain.resources.Types.*

final case class Product(id: ProductId, name: String, tasksList: List[TaskId])

final case class Order(id: OrderId, quantity: OrderQuantity, productId: ProductId)

final case class Task(id: TaskId, time: TaskTime, physicalResources: List[String])

final case class HumanResource(id: HumanResourceId, name: String, physicalResources: List[String] )

final case class PhysicalResource(id: PhysicalResourceId, name: String)

//TODO: MUDAR PRODUCTNUMBER START, END PARA NOVO SIMPLE TYPE "POSITIVEINTEGER"
final case class TaskSchedule(orderId: OrderId, productNumber: Int, taskId:TaskId, start:Int, end:Int,
                              physicalResourceIds: List[PhysicalResourceId], humanResourceNames: List[String])
