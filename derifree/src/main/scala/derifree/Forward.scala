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

  def withBorrow(borrow: YieldCurve[T]): Forward[T]

  def withDiscount(discount: YieldCurve[T]): Forward[T]

  def withDivs(divs: List[Dividend[T]]): Forward[T]

  def withSpot(spot: Double): Forward[T]

object Forward:

  def apply[T: TimeLike](
      spot: Double,
      divs: List[Dividend[T]],
      discount: YieldCurve[T],
      borrow: YieldCurve[T]
  ): Forward[T] = impl(spot, divs, discount, borrow)

  // reference implementation: correct but slower
  private[derifree] def referenceImpl[T: TimeLike](
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
      divs match
        case Nil => Nil
        case h :: t =>
          val div0 = h
          val r0 =
            (1 - div0.prop) * borrow.discountFactor(div0.exDiv) / discount.discountFactor(
              div0.exDiv
            )
          (divs zip t).scanLeft(r0):
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
      override def spot: Double = spot0

      override def apply(t: T): Double = s0Star * r(t) + d(t)

      override def dividendFloor(t: T): Double = d(t)

      override def dividends: List[Dividend[T]] = divs

      override def withDivs(newDivs: List[Dividend[T]]): Forward[T] =
        referenceImpl[T](spot, newDivs, discount, borrow)

      override def withBorrow(newBorrow: YieldCurve[T]): Forward[T] =
        referenceImpl[T](spot, divs, discount, newBorrow)

      override def withDiscount(newDiscount: YieldCurve[T]): Forward[T] =
        referenceImpl[T](spot, divs, newDiscount, borrow)

      override def withSpot(newSpot: Double): Forward[T] =
        referenceImpl[T](newSpot, divs, discount, borrow)

  private[derifree] def impl[T: TimeLike](
      spot0: Double,
      divs: List[Dividend[T]],
      discount: YieldCurve[T],
      borrow: YieldCurve[T]
  ): Forward[T] =

    val divsArr = divs.toArray
    val nDivs = divsArr.length

    def r(t: T): Double =
      var i = 0
      var acc = 1.0
      while (i < nDivs && divsArr(i).exDiv <= t) {
        acc *= (1 - divsArr(i).prop)
        i += 1
      }
      acc * borrow.discountFactor(t) / discount.discountFactor(t)

    val rs: Array[Double] =
      (divs match
        case Nil => Nil
        case h :: t =>
          val div0 = h
          val r0 =
            (1 - div0.prop) * borrow.discountFactor(div0.exDiv) / discount.discountFactor(
              div0.exDiv
            )
          (divs zip t).scanLeft(r0):
            case (acc, (div1, div2)) =>
              acc * (1 - div2.prop) * borrow.discountFactor(div1.exDiv, div2.exDiv) / discount
                .discountFactor(
                  div1.exDiv,
                  div2.exDiv
                )
      ).toArray

    def d(t: T): Double =
      var i = nDivs - 1
      var acc = 0.0
      while (i >= 0 && divsArr(i).exDiv > t) {
        acc += divsArr(i).cash / rs(i)
        i -= 1
      }
      r(t) * acc

    val sumOfAlphaStar: Double = (divs zip rs).map(_.cash / _).sum

    val s0Star: Double = spot0 - sumOfAlphaStar

    new Forward[T]:
      override def spot: Double = spot0

      override def apply(t: T): Double = s0Star * r(t) + d(t)

      override def dividendFloor(t: T): Double = d(t)

      override def dividends: List[Dividend[T]] = divs

      override def withDivs(newDivs: List[Dividend[T]]): Forward[T] =
        impl[T](spot, newDivs, discount, borrow)

      override def withBorrow(newBorrow: YieldCurve[T]): Forward[T] =
        impl[T](spot, divs, discount, newBorrow)

      override def withDiscount(newDiscount: YieldCurve[T]): Forward[T] =
        impl[T](spot, divs, newDiscount, borrow)

      override def withSpot(newSpot: Double): Forward[T] =
        impl[T](newSpot, divs, discount, borrow)
