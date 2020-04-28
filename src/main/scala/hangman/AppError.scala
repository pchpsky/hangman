package hangman

sealed trait AppError extends Exception {
  def message: String
}

final case class ResourceNotFound(resource: String) extends AppError {
  override def message: String = s"Resource $resource not found"
}

case object EmptyDictionary extends AppError {
  override def message: String = "Dictionary is empty"
}
