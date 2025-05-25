package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.*

object TaskScheduleGenerator extends Properties("TaskSchedule"):

  def generateDomainData: Gen[(List[Order], List[Product], List[Task], List[HumanResource], List[PhysicalResource])] =
    for {
      physicalResources <- PhysicalResourceGenerator.generatePhysicalResourcesList
      types = PhysicalResourceGenerator.generatePhysicalTypesListFromResources(physicalResources)

      tasks <- TaskGenerator.generateTaskList(types)
      humanResources <- HumanResourceGenerator.generateHumanResourcesList(types)
      products <- ProductGenerator.generateProductsList(tasks)
      orders <- OrderGenerator.generateOrdersList(products)
    } yield (orders, products, tasks, humanResources, physicalResources)


  def generateDeterministicDomainData: Gen[(List[Order], List[Product], List[Task], List[HumanResource], List[PhysicalResource])] =
    for {
      physicalResources <- PhysicalResourceGenerator.generatePhysicalResourcesList
      types = PhysicalResourceGenerator.generatePhysicalTypesListFromResources(physicalResources)

      humanResources <- HumanResourceGenerator.generateDeterministicHumanResourcesList(types)
      tasks <- TaskGenerator.generateDeterministicTaskList(humanResources, physicalResources)
      products <- ProductGenerator.generateProductsList(tasks)
      orders <- OrderGenerator.generateOrdersList(products)
    } yield (orders, products, tasks, humanResources, physicalResources)