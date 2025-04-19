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
  
  case EmptyPhysicalResourceType(error: String) //TODO: Create a xml input and output file with empty resource type
  case PhysicalResourceTypeNotFound(error: String) //TODO: Create a xml input and output file with a task that has an non existing Physical Resource Type

  case ProductDoesNotExist(error: String)
  case TaskDoesNotExist(error: String)

  case InvalidQuantity(error: String)
  case InvalidTime(error: String)   //TODO: Create a xml input and output file with time <= 0
  
  case TaskUsesNonExistentPRT(error: String)
  case ResourceUnavailable(task: String, physicalResource: String)

