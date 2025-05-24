package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.{Order, Product}
import pj.generators.SimpleTypeGenerator.{OrderIdGenerator, OrderQuantityGenerator}

object OrderGenerator extends Properties("Order"):

  def generateOrder(products: List[Product]): Gen[Order] =
    for {
      orderId <- OrderIdGenerator
      quantity <- OrderQuantityGenerator
      productId <- Gen.oneOf(products.map(_.id))
    } yield Order(orderId, quantity, productId)

  def generateOrdersList: List[Product] => Gen[List[Order]] =
    (products) => Gen.nonEmptyListOf(generateOrder(products))
