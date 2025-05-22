package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.Order
import pj.domain.resources.Types.ProductId
import pj.generators.SimpleTypeGenerator.{OrderIdGenerator, OrderQuantityGenerator, ProductIdGenerator}

object OrderGenerator extends Properties("Order"):

  def generateOrder(productId: ProductId): Gen[Order] =
    for {
      orderId <- OrderIdGenerator
      quantity <- OrderQuantityGenerator
    } yield Order(orderId, quantity, productId)
