package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.{Product, Task}
import pj.generators.SimpleTypeGenerator.{ProductIdGenerator, ProductNameGenerator, TaskIdGenerator}

object ProductGenerator extends Properties("Product"):

  def generateProduct(taskList: List[Task]): Gen[Product] =
    for {
      productId <- ProductIdGenerator
      name <- ProductNameGenerator
      tasks <- Gen.someOf(taskList).map(_.toList).suchThat(_.nonEmpty)
    } yield Product(productId, name, tasks.map(_.id))
