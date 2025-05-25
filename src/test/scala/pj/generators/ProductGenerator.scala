package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.{Product, Task}
import pj.generators.SimpleTypeGenerator.{ProductIdGenerator, ProductNameGenerator}

object ProductGenerator extends Properties("Product"):

  def generateProduct(taskList: List[Task]): Gen[Product] =
    for {
      productId <- ProductIdGenerator
      name <- ProductNameGenerator
      tasks <- Gen.nonEmptyListOf(Gen.oneOf(taskList.map(_.id)))
    } yield Product(productId, name, tasks.distinct)

  val generateProductsList: List[Task] => Gen[List[Product]] =
    (tasks) => Gen.nonEmptyListOf(generateProduct(tasks))
