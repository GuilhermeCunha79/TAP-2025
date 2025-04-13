package pj.domain.resources

import pj.domain.resources.Types.*

  final case class Product(id: ProductId, name: String)

  final case class Order(id: OrderId, quantity: Int, productId: ProductId)

  final case class Task(id: TaskId, time: Int)

  final case class Human(id: HumanId, name: String)

  final case class PhysicalResource(id: PhysicalId, name: String)

