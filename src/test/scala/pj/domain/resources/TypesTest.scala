package pj.domain.resources

import org.scalatest.funsuite.AnyFunSuite
import pj.domain.DomainError.InvalidProductId
import pj.domain.resources.Types.ProductId

class TypesTest extends AnyFunSuite:
  
  test("ProductId.from with invalid prefix"):
    val result = ProductId.from("PXD_123")
    assert(result == Left(InvalidProductId("PXD_123")))