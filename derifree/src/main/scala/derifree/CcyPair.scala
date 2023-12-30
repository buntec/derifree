package derifree

import cats.Show

final case class CcyPair(foreign: Ccy, domestic: Ccy):
  override def toString(): String = s"$foreign$domestic"

object CcyPair:

  given Show[CcyPair] = Show.fromToString[CcyPair]
