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
    Gen.nonEmptyListOf(PhysicalResourceGenerator.generatePhysicalResource)

  def generatePhysicalTypesListFromResources: List[PhysicalResource] => List[PhysicalResourceType] =
    _.map(_.physical_type).distinct
