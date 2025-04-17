package pj.domain.resources

import pj.domain.resources.Types.*

final case class Product(id: ProductId, name: String, tasksList: List[TaskId])

final case class Order(id: OrderId, quantity: OrderQuantity, productId: ProductId)

final case class Task(id: TaskId, time: TaskTime, physicalResources: List[PhysicalResourceType])

final case class HumanResource(id: HumanResourceId, name: String, physicalResources: List[PhysicalResourceType] )

final case class PhysicalResource(id: PhysicalResourceId, name: PhysicalResourceType)


final case class TaskSchedule(orderId: OrderId, productNumber: Int, taskId:TaskId, start:TaskTime, end:TaskTime,
                              physicalResourceIds: List[PhysicalResourceId], humanResourceNames: List[String])
