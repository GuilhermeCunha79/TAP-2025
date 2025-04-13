package pj.xml

import pj.domain.Result
import pj.domain.resources.*
import pj.domain.resources.Types.*

import scala.xml.{Elem, Node}

object XMLToDomain :

  def getProduct(xml: Node): Result[Product] =
    for
      id <- XML.fromAttribute(xml, "id")
      name <- XML.fromAttribute(xml, "name")
      productId <- ProductId.from(id)
    yield Product(productId, name)



