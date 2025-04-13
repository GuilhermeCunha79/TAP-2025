import pj.domain.*
import pj.io.FileIO
import pj.xml.*

//val filePath = "C:\\Users\\Utilizador\\Documents\\TAP_Project\\files\\assessment\\ms01\\validAgenda_01_in.xml"
val filePath = "C:\\Users\\migue\\Desktop\\Universidade\\Mestrado GitHub Projects\\2 Ano\\2 Semestre\\TAP\\tap-pj-ncf-12\\files\\assessment\\ms01\\validAgenda_01_in.xml"

val xmlResult = FileIO.load(filePath)

xmlResult match {
  case Right(production) => {
    val physicalResources = XML.fromNode(production, "PhysicalResources") match {
      case Right(node) =>
        XML.traverse((node \ "Physical"), { physical =>
          for {
            id <- XML.fromAttribute(physical, "id")
            typ <- XML.fromAttribute(physical, "type")
          } yield (id, typ)
        })
      case Left(error) => Left(error)
    }
    
    val products = XML.fromNode(production, "Products") match {
      case Right(node) =>
        XML.traverse((node \ "Product"), { product =>
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

