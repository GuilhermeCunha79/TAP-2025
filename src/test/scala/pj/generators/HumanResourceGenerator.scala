package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.HumanResource
import pj.domain.resources.Types.PhysicalResourceType
import pj.generators.SimpleTypeGenerator.{HumanResourceIdGenerator, HumanResourceNameGenerator}

object HumanResourceGenerator extends Properties("HumanResource"):
  
  def generateHumanResource(availableTypes: List[PhysicalResourceType]): Gen[HumanResource] =
    for {
      hrId <- HumanResourceIdGenerator
      name <- HumanResourceNameGenerator
      canOperate <- Gen.nonEmptyListOf(Gen.oneOf(availableTypes))
    } yield HumanResource(hrId, name, canOperate)

  def generateHumanResourcesList: List[PhysicalResourceType] => Gen[List[HumanResource]] =
    (types) => Gen.listOfN(types.size, generateHumanResource(types))


  def generateDeterministicHumanResource(availableTypes: List[PhysicalResourceType]): Gen[HumanResource] =
    for {
      hrId <- HumanResourceIdGenerator
      name <- HumanResourceNameGenerator
    } yield HumanResource(hrId, name, availableTypes)
  
  def generateDeterministicHumanResourcesList: List[PhysicalResourceType] => Gen[List[HumanResource]] =
    (types) => Gen.listOfN(types.size, generateDeterministicHumanResource(types))
