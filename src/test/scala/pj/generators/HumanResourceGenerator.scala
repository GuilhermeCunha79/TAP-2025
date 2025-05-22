package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.HumanResource
import pj.domain.resources.Types.PhysicalResourceType
import pj.generators.SimpleTypeGenerator.{HumanResourceIdGenerator, HumanResourceNameGenerator, PhysicalResourceTypeGenerator}

object HumanResourceGenerator extends Properties("HumanResource"):
  
  def generateHumanResource(availableTypes: List[PhysicalResourceType]): Gen[HumanResource] =
    for {
      hrId <- HumanResourceIdGenerator
      name <- HumanResourceNameGenerator
      canOperate <- Gen.someOf(availableTypes).map(_.toList).suchThat(_.nonEmpty)
    } yield HumanResource(hrId, name, canOperate)