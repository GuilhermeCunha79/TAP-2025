package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.PhysicalResource
import pj.domain.resources.Types.PhysicalResourceType
import pj.generators.SimpleTypeGenerator.{PhysicalResourceIdGenerator, PhysicalResourceTypeGenerator}

object PhysicalResourceGenerator extends Properties("PhysicalResource"):
  
  def generatePhysicalResource: Gen[PhysicalResource] =
    for {
      prId <- PhysicalResourceIdGenerator
      prType <- PhysicalResourceTypeGenerator
    } yield PhysicalResource(prId, prType)

  def generatePhysicalResourcesList: Gen[List[PhysicalResource]] =
    Gen.nonEmptyListOf(generatePhysicalResource)

  def generatePhysicalTypesListFromResources(resources: List[PhysicalResource]): List[PhysicalResourceType] =
    resources.map(_.physical_type).distinct

