package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.{HumanResource, PhysicalResource}
import pj.domain.resources.Types.PhysicalResourceType
import pj.generators.SimpleTypeGenerator.{HumanResourceIdGenerator, HumanResourceNameGenerator, PhysicalResourceIdGenerator, PhysicalResourceTypeGenerator}

object PhysicalResourceGenerator extends Properties("PhysicalResource"):
  
  def generatePhysicalResource: Gen[PhysicalResource] =
    for {
      prId <- PhysicalResourceIdGenerator
      prType <- PhysicalResourceTypeGenerator
    } yield PhysicalResource(prId, prType)