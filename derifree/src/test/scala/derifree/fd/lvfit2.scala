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
package fd

import cats.effect.IO
import cats.syntax.all.*
import derifree.dtos.etd.options.OptionQuote
import derifree.dtos.etd.options.Snapshot
import derifree.testutils.*

import scala.concurrent.duration.*

class LVFitSuite2 extends munit.CatsEffectSuite:

  override def munitIOTimeout: Duration = 5.minutes

  test("fit local vol from option market snapshot"):
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

        val lvFitter = LocalVolFitter.apply
        val lvSettings = LocalVolFitter.Settings(5, 0.01, 3.0)

        val initialBorrow = YieldCurve.zero[YearFraction]
        val divs = Nil

        for
          spot <- IO.fromOption(
            (stock.underlying.bid, stock.underlying.ask).mapN((bid, ask) => (bid + ask) / 2)
          )(Exception("missing spot"))
          discount <- IO.fromEither(discountFitter.fromSnapshot(spx, refTime))
          forward = Forward[YearFraction](spot.toDouble, divs, discount, initialBorrow)
          borrow <- IO.fromEither(borrowFitter.fromSnapshot(stock, forward, discount, refTime))
          quotes = stock.quotes
            .map: q =>
              val expiry = q.expiry.atTime(16, 0).atZone(stock.expiryZone).toInstant

              val timeToMaturity = TimeLike[java.time.Instant]
                .yearFractionBetween(stock.timestamp.toInstant, expiry)

              val ivBid = derifree.etd.options.ImpliedVol.american(
                q.bid.toDouble,
                q.strike.toDouble,
                timeToMaturity,
                q.isCall,
                refTime,
                forward,
                discount
              )

              val ivAsk = derifree.etd.options.ImpliedVol.american(
                q.ask.toDouble,
                q.strike.toDouble,
                timeToMaturity,
                q.isCall,
                refTime,
                forward,
                discount
              )

              (ivBid, ivAsk).tupled.fold(
                _ => q.copy(impliedVol = None),
                (bid, ask) =>
                  q.copy(impliedVol =
                    OptionQuote
                      .ImpliedVol(bid = bid.some, ask = ask.some, mid = ((bid + ask) / 2).some)
                      .some
                  )
              )
          snapshot = stock.copy(quotes = quotes)
          lv <- IO.fromEither(
            lvFitter.fitSnapshot(
              snapshot,
              forward,
              refTime,
              lvSettings,
              YearFraction.oneDay * 7
            )
          )
          _ <- IO.println(lv)
        yield ()
    )
