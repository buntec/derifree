package derifree.literals

import cats.syntax.all.*
import org.typelevel.literally.Literally

import java.time.format.DateTimeParseException

extension (inline ctx: StringContext)
  inline def i(inline args: Any*): java.time.Instant =
    ${ InstantLiteral('ctx, 'args) }

object InstantLiteral extends Literally[java.time.Instant]:
  def validate(s: String)(using Quotes) =
    Either.catchOnly[DateTimeParseException](java.time.Instant.parse(s)) match
      case Left(e)  => Left(s"Invalid instant literal: ${e.getMessage}.")
      case Right(_) => Right('{ java.time.Instant.parse(${ Expr(s) }) })
