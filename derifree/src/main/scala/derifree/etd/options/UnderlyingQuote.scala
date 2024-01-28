package derifree.etd.options

import io.circe.Codec
import io.circe.generic.semiauto._

case class UnderlyingQuote(
    ticker: Ticker,
    last: BigDecimal,
    lastTrade: Option[java.time.OffsetDateTime],
    bid: Option[BigDecimal],
    ask: Option[BigDecimal]
)

object UnderlyingQuote:
  given Codec[UnderlyingQuote] = deriveCodec[UnderlyingQuote]
