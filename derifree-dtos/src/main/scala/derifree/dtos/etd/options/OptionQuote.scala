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

package derifree.dtos.etd.options

import io.circe.Codec
import io.circe.generic.semiauto._

import OptionQuote.*

case class OptionQuote(
    optionType: OptionType,
    strike: BigDecimal,
    expiry: java.time.LocalDate,
    bid: BigDecimal,
    ask: BigDecimal,
    last: Option[BigDecimal],
    lastTrade: Option[java.time.OffsetDateTime],
    volume: Option[Int],
    openInterest: Option[Int],
    delta: Option[Double],
    gamma: Option[Double],
    impliedVol: Option[ImpliedVol]
):
  def mid: BigDecimal = (bid + ask) / 2

  def spread: BigDecimal = bid - ask

  def isCall: Boolean = optionType match
    case OptionType.Call => true
    case OptionType.Put  => false

  def isPut: Boolean = !isCall

object OptionQuote:

  // The `mid` may or may not be equal to the average of `bid` and `ask` and is
  // therefore not necessarily redundant.
  case class ImpliedVol(
      bid: Option[Double] = None,
      mid: Option[Double] = None,
      ask: Option[Double] = None
  )

  object ImpliedVol:
    given Codec[ImpliedVol] = deriveCodec[ImpliedVol]

  given Codec[OptionQuote] = deriveCodec[OptionQuote]
