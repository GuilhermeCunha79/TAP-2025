import pj.domain.Result
import pj.domain.resources.PhysicalResource
import pj.xml.{XML, XMLToDomain}
import pj.io.FileIO
import pj.xml.XMLToDomain.getProduct
import scala.xml.*


//val xml = FileIO.load("C:\\Users\\Utilizador\\Documents\\TAP_Project\\files\\assessment\\ms01\\validAgenda_01_in.xml")
//val result = XML.traverse( XML.fromNode(xml, "PhysicalResources"))

val filePath = "C:\\Users\\Utilizador\\Documents\\TAP_Project\\files\\assessment\\ms01\\validAgenda_01_in.xml"

val xmlResult = FileIO.load(filePath)

xmlResult match {
  case Right(production) => {
    val physicalResources = pj.xml.XML.fromNode(production, "PhysicalResources") match {
      case Right(node) =>
        pj.xml.XML.traverse((node \ "Physical"), { physical =>
          for {
            id <- pj.xml.XML.fromAttribute(physical, "id")
            typ <- pj.xml.XML.fromAttribute(physical, "type")
          } yield (id, typ)
        })
      case Left(error) => Left(error)
    }
    val products = pj.xml.XML.fromNode(production, "Products") match {
      case Right(node) =>
        pj.xml.XML.traverse((node \ "Product"), { product =>
          XMLToDomain.getProduct(product) match {
            case Right(product) => Right(product)
            case Left(error) => Left(error)
          }
        })
      case Left(error) => Left(error)
    }

    physicalResources match {
      case Right(resources) => println(s"Successfully parsed resources: $resources")
      case Left(error) => println(s"Error parsing resources: $error")
    }
    products match {
      case Right(products) => println(s"Successfully parsed resources: $products")
      case Left(error) => println(s"Error parsing resources: $error")
    }
  }
  case Left(error) => println(s"Failed to load XML file: $error")
}

