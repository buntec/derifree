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

package derifree

import cats.syntax.all.*

trait Forward[T]:

  def spot: Double

  def apply(t: T): Double

  def dividends: List[Dividend[T]]

  def dividendFloor(t: T): Double

object Forward:

  def buehler[T: TimeLike](
      spot0: Double,
      divs: List[Dividend[T]],
      discount: YieldCurve[T],
      borrow: YieldCurve[T]
  ): Forward[T] =
    // We mimick notation from Buehler's "Volatility and Dividends" paper.

    // R(t) = \exp(\int_0^t (r_s - y_s) ds) \prod_{j: \tau_j \leq t} (1 - \beta_j)
    def r(t: T): Double =
      borrow.discountFactor(t) / discount.discountFactor(t) * divs
        .takeWhile(_.exDiv <= t)
        .map(_.prop)
        .foldLeft(1.0)((acc, beta) => acc * (1 - beta))

    // R(t) evaluated at all div times: R(\tau_1), ..., R(\tau_n)
    val rs: List[Double] =
      val div0 = divs.head
      val r0 = (1 - div0.prop) * borrow.discountFactor(div0.exDiv) / discount.discountFactor(
        div0.exDiv
      )
      (divs zip divs.tail).scanLeft(r0):
        case (acc, (div1, div2)) =>
          acc * (1 - div2.prop) * borrow.discountFactor(div1.exDiv, div2.exDiv) / discount
            .discountFactor(
              div1.exDiv,
              div2.exDiv
            )

    // the dividend floor D(t) = \sum_{j: \tau_j > t} \alph_j / R(t, \tau_j)
    // we use the fact that R(t, \tau_j) = R(\tau_j) / R(t)
    def d(t: T): Double =
      r(t) * (divs zip rs)
        .dropWhile((div, _) => div.exDiv <= t)
        .foldLeft(0.0):
          case (acc, (div, r)) => acc + div.cash / r

    // \sum_{j=1}^n \alpha_j^\star, where \alpha_j^\star = \alpha_j / R(\tau_j)
    val sumOfAlphaStar: Double = (divs zip rs).map(_.cash / _).sum

    // S_0^\star = S_0 - \sum_{j=1}^n \alpha_j^\star
    val s0Star: Double = spot0 - sumOfAlphaStar

    new Forward[T]:
      def spot: Double = spot0
      def apply(t: T): Double = s0Star * r(t) + d(t)
      def dividendFloor(t: T): Double = d(t)
      def dividends: List[Dividend[T]] = divs
