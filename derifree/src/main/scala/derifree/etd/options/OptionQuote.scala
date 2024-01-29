/*
 * Copyright 2023 buntec
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
