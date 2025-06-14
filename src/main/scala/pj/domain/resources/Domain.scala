package pj.domain.resources

import pj.domain.resources.Types.*

final case class Product(id: ProductId, name: ProductName, tasksList: List[TaskId])

final case class Order(id: OrderId, quantity: OrderQuantity, productId: ProductId)

final case class Task(id: TaskId, time: TaskTime, physicalResourceTypes: List[PhysicalResourceType])

final case class HumanResource(id: HumanResourceId, name: HumanResourceName, physicalResourceTypes: List[PhysicalResourceType] )

final case class PhysicalResource(id: PhysicalResourceId, physical_type: PhysicalResourceType)

final case class TaskSchedule(orderId: OrderId, productNumber: ProductNumber, taskId:TaskId, 
                              start:TaskScheduleTime, end:TaskScheduleTime,
                              physicalResourceIds: List[PhysicalResourceId], humanResourceNames: List[HumanResourceName])

final case class TaskInfo(orderId: OrderId, productNumber: ProductNumber, taskId: TaskId,
                          task: Task, earliestStart: EarliestStartTime, productTaskIndex: ProductTaskIndex)

final case class SchedulingState(readyTasks: List[TaskInfo], resourceAvailability: Map[String, Int],
                                 schedules: List[TaskSchedule], productProgress: Map[(OrderId, ProductNumber), Int])