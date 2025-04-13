package pj.domain.resources

import pj.domain.resources.Types.*

final case class Product(id: ProductId, name: String)

final case class Order(id: OrderId, quantity: OrderQuantity, productId: ProductId)

final case class Task(id: TaskId, time: TaskTime, physicalResources: List[PhysicalResourceType])

final case class Human(id: HumanResourceId, name: String)

final case class PhysicalResource(id: PhysicalResourceId, name: PhysicalResourceType)
