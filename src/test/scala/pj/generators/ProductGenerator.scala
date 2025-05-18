package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.Product
import pj.generators.SimpleTypeGenerator.{ProductIdGenerator, ProductNameGenerator, TaskIdGenerator}

object ProductGenerator extends Properties("Product"):

  def generateProduct: Gen[Product] =
    for {
      productId <- ProductIdGenerator
      name <- ProductNameGenerator
      tasks <- Gen.nonEmptyListOf(TaskIdGenerator)
    } yield Product(productId, name, tasks)
