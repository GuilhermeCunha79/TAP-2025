package pj.generators

import org.scalacheck.{Gen, Properties}
import pj.domain.resources.Order
import pj.generators.SimpleTypeGenerator.{OrderIdGenerator, OrderQuantityGenerator, ProductIdGenerator}

object OrderGenerator extends Properties("Order"):

  def generateOrder: Gen[Order] =
    for {
      orderId <- OrderIdGenerator
      quantity <- OrderQuantityGenerator
      productId <- ProductIdGenerator
    } yield Order(orderId, quantity, productId)
