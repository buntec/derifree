package derifree

import scala.util.control.NoStackTrace

abstract class Error extends NoStackTrace

object Error:
  case class Generic(override val getMessage: String) extends Error
  case class MissingData(override val getMessage: String) extends Error
  case class BadNumerics(override val getMessage: String) extends Error
