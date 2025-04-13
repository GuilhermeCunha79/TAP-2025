import pj.domain.*
import pj.domain.resources.Types.*
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
          XMLToDomain.getPhysicalResource(physical) match {
            case Right(physical) => Right(physical)
            case Left(error) => Left(error)
          }
        })
      case Left(error) => Left(error)
    }

    val physicalResourcesTypes: Result[List[PhysicalResourceType]] =
      physicalResources.map(_.map(_.name).distinct)

    val tasks = physicalResourcesTypes match {
      case Right(resourceTypes) => // Extract the list from the Right
        XML.fromNode(production, "Tasks") match {
          case Right(node) =>
            XML.traverse((node \ "Task"), { task =>
              XMLToDomain.getTask(resourceTypes)(task) match {
                case Right(task) => Right(task)
                case Left(error) => Left(error)
              }
            })
          case Left(error) => Left(error)
        }
      case Left(error) => Left(error)
    }

    val humanResources = physicalResourcesTypes match {
      case Right(resourceTypes) =>
        XML.fromNode(production, "HumanResources") match {
          case Right(node) =>
            XML.traverse((node \ "Human"), { human =>
              XMLToDomain.getHumanResource(resourceTypes)(human) match {
                case Right(human) => Right(human)
                case Left(error) => Left(error)
              }
            })
          case Left(error) => Left(error)
        }
      case Left(error) => Left(error)
    }

    val products = tasks match {
      case Right(tasks) =>
        XML.fromNode(production, "Products") match {
          case Right(node) =>
            XML.traverse((node \ "Product"), { product =>
              XMLToDomain.getProduct(tasks)(product) match {
                case Right(product) => Right(product)
                case Left(error) => Left(error)
              }
            })
          case Left(error) => Left(error)
        }
      case Left(error) => Left(error)
    }

    val orders = XML.fromNode(production, "Orders") match {
      case Right(node) =>
        XML.traverse((node \ "Order"), { order =>
          XMLToDomain.getOrder(order) match {
            case Right(order) => Right(order)
            case Left(error) => Left(error)
          }
        })
      case Left(error) => Left(error)
    }

    physicalResources match {
      case Right(resources) => println(s"Successfully parsed resources: $resources")
      case Left(error) => println(s"Error parsing resources: $error")
    }

    humanResources match {
      case Right(resources) => println(s"Successfully parsed resources: $resources")
      case Left(error) => println(s"Error parsing resources: $error")
    }

    tasks match {
      case Right(resources) => println(s"Successfully parsed resources: $resources")
      case Left(error) => println(s"Error parsing resources: $error")
    }

    products match {
      case Right(products) => println(s"Successfully parsed resources: $products")
      case Left(error) => println(s"Error parsing resources: $error")
    }

    orders match {
      case Right(orders) => println(s"Successfully parsed resources: $orders")
      case Left(error) => println(s"Error parsing resources: $error")
    }
  }
  case Left(error) => println(s"Failed to load XML file: $error")
}

