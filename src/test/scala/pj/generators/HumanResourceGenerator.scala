package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.HumanResource
import pj.generators.SimpleTypeGenerator.{HumanResourceIdGenerator, HumanResourceNameGenerator, PhysicalResourceTypeGenerator}

object HumanResourceGenerator extends Properties("HumanResource"):
  
  def generateHumanResource: Gen[HumanResource] =
    for {
      hrId <- HumanResourceIdGenerator
      name <- HumanResourceNameGenerator
      canOperate <- Gen.nonEmptyListOf(PhysicalResourceTypeGenerator)
    } yield HumanResource(hrId, name, canOperate)