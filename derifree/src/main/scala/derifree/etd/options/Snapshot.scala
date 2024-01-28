package derifree.etd
package options

import io.circe.Codec
import io.circe.generic.semiauto._

case class Snapshot(
    timestamp: java.time.OffsetDateTime,
    underlying: UnderlyingQuote,
    exerciseStyle: ExerciseStyle,
    exchange: Exchange,
    expiryZone: java.time.ZoneId,
    quotes: List[OptionQuote]
)

object Snapshot:
  given Codec[Snapshot] = deriveCodec[Snapshot]
