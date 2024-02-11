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

import cats.Show
import cats.effect.IO
import cats.syntax.all.*
import derifree.dtos.etd.options.OptionQuote
import derifree.dtos.etd.options.Snapshot
import derifree.syntax.*
import derifree.testutils.*

import scala.concurrent.duration.*

class EtdSuite extends munit.CatsEffectSuite:

  override def munitIOTimeout: Duration = 5.minutes

  test("fit USD discount curve from SPX snapshot"):
    readJsonResource[IO, Snapshot]("SPX-2024-02-07.json")
      .flatMap: snapshot =>
        val fitter = derifree.etd.options.DiscountFitter[YearFraction](
          derifree.etd.options.DiscountFitter.Settings()
        )
        val yc = fitter.fromSnapshot(snapshot, YearFraction.zero)
        IO.fromEither(yc)
          .flatMap: yc =>
            val yfs = List(1, 30, 90, 180, 365, 3 * 365)
              .map(days => YearFraction.oneDay * days)
            yfs.traverse_(t => IO.println(s"t=$t, r=${yc.spotRate(t)}"))

  test("compute implied vols from option market snapshot"):
    (
      readJsonResource[IO, Snapshot]("SPX-2024-02-07.json"),
      readJsonResource[IO, Snapshot]("AMZN-2024-02-07.json")
    ).flatMapN: (spx, amzn) =>
      val refTime = YearFraction.zero
      val discountFitter = derifree.etd.options.DiscountFitter[YearFraction](
        derifree.etd.options.DiscountFitter.Settings()
      )
      val borrow = YieldCurve.zero[YearFraction]
      val divs = Nil
      for
        spot <- IO.fromOption(
          (amzn.underlying.bid, amzn.underlying.ask).mapN((bid, ask) => (bid + ask) / 2)
        )(Exception("missing spot"))
        discount <- IO.fromEither(discountFitter.fromSnapshot(spx, refTime))
        forward = Forward[YearFraction](spot.toDouble, divs, discount, borrow)
        quotes = amzn.quotes
          .map: q =>
            val expiry = q.expiry.atTime(16, 0).atZone(amzn.expiryZone).toInstant
            val timeToMaturity = TimeLike[java.time.Instant]
              .yearFractionBetween(amzn.timestamp.toInstant, expiry)
            val iv = derifree.etd.options.ImpliedVol.american(
              q.mid.toDouble,
              q.strike.toDouble,
              timeToMaturity,
              q.isCall,
              refTime,
              forward,
              discount
            )
            iv.fold(
              _ => q.copy(impliedVol = None),
              vol => q.copy(impliedVol = OptionQuote.ImpliedVol().copy(mid = vol.some).some)
            )
      // _ <- IO.println(quotes)
      yield ()

  test("fit borrow curve from option market snapshot"):

    List(
      "AMZN-2024-02-07.json",
      "META-2024-02-07.json",
      "TSLA-2024-02-07.json",
      "GOOG-2024-02-07.json"
    ).traverse(fileName =>
      (
        readJsonResource[IO, Snapshot]("SPX-2024-02-07.json"),
        readJsonResource[IO, Snapshot](fileName)
      ).flatMapN: (spx, stock) =>
        val refTime = YearFraction.zero

        val discountFitter = derifree.etd.options.DiscountFitter[YearFraction](
          derifree.etd.options.DiscountFitter.Settings()
        )

        val borrowFitter = derifree.etd.options.BorrowFitter[YearFraction](
          derifree.etd.options.BorrowFitter.Settings()
        )

        val initialBorrow = YieldCurve.zero[YearFraction]

        val divs = Nil

        for
          spot <- IO.fromOption(
            (stock.underlying.bid, stock.underlying.ask).mapN((bid, ask) => (bid + ask) / 2)
          )(Exception("missing spot"))
          discount <- IO.fromEither(discountFitter.fromSnapshot(spx, refTime))
          forward = Forward[YearFraction](spot.toDouble, divs, discount, initialBorrow)
          borrow <- IO.fromEither(borrowFitter.fromSnapshot(stock, forward, discount, refTime))
          _ <- List(1, 7, 30, 90, 180, 365, 2 * 365, 3 * 365).traverse_(d =>
            val r = borrow.spotRate(refTime.plusDays(d))
            IO.println(s"d=$d, r=$r")
          )
          spotAndBorrow <- IO.fromEither(
            borrowFitter.fromSnapshotCorrectSpot(
              stock,
              forward,
              discount,
              refTime
            )
          )
          (spot, borrow) = spotAndBorrow
          _ <- IO.println(s"spot=${forward.spot}, corrected spot=${spot}")
          _ <- List(1, 7, 30, 90, 180, 365, 2 * 365, 3 * 365).traverse_(d =>
            val r = borrow.spotRate(refTime.plusDays(d))
            IO.println(s"d=$d, r=$r")
          )
        yield ()
    )
