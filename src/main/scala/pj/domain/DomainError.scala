package pj.domain

type Result[A] = Either[DomainError,A]

enum DomainError:
  case IOFileProblem(error: String)
  case XMLError(error: String)
  case InvalidProductId(error: String)
  case InvalidPhysicalId(error: String)
  case InvalidOrderId(error: String)
  case InvalidHumanId(error: String)
  case InvalidTaskId(error: String)

  case EmptyPhysicalResourceType(error: String)
  case PhysicalResourceTypeNotFound(humanResourceId: String, physicalResourceType: String)

  case ProductDoesNotExist(error: String)
  case TaskDoesNotExist(error: String)

  case InvalidQuantity(error: String)
  case InvalidTime(error: String)

  case TaskUsesNonExistentPRT(error: String)
  case ResourceUnavailable(task: String, physicalResource: String)

