package derifree.etd.options

import io.circe.Codec
import io.circe.generic.semiauto._

case class OptionQuote(
    tpe: OptionType,
    strike: BigDecimal,
    expiry: java.time.LocalDate,
    last: Option[BigDecimal],
    lastTrade: Option[java.time.OffsetDateTime],
    bid: BigDecimal,
    ask: BigDecimal,
    volume: Option[Int],
    openInterest: Option[Int],
    delta: Option[Double],
    gamma: Option[Double],
    impliedVol: Option[Double]
):
  def mid: BigDecimal = (bid + ask) / 2

  def spread: BigDecimal = bid - ask

  def isCall: Boolean = tpe match
    case OptionType.Call => true
    case OptionType.Put  => false

  def isPut: Boolean = !isCall

object OptionQuote:
  given Codec[OptionQuote] = deriveCodec[OptionQuote]
