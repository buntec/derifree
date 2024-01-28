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
    volume: Int,
    openInterest: Int,
    delta: Double,
    gamma: Double,
    impliedVol: Option[Double]
):
  def mid: BigDecimal = (bid + ask) / 2

  def spread: BigDecimal = bid - ask

  def isEuropean = tpe match
    case OptionType.AmericanCall => false
    case OptionType.AmericanPut  => false
    case OptionType.EuropeanCall => true
    case OptionType.EuropeanPut  => true

  def isAmerican = !isEuropean

  def isCall: Boolean = tpe match
    case OptionType.AmericanCall | OptionType.EuropeanCall => true
    case OptionType.AmericanPut | OptionType.EuropeanPut   => false

  def isPut: Boolean = !isCall

object OptionQuote:
  given Codec[OptionQuote] = deriveCodec[OptionQuote]
