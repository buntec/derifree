package derifree
package fd

import cats.effect.IO
import cats.syntax.all.*
import derifree.dtos.etd.options.OptionQuote
import derifree.dtos.etd.options.Snapshot
import derifree.testutils.*

import scala.concurrent.duration.*
import derifree.fd.LocalVolFitter.PureObservation

class LVFitSuite2 extends munit.CatsEffectSuite:

  override def munitIOTimeout: Duration = 5.minutes

  test("fit local vol from option market snapshot"):
    (
      readJsonResource[IO, Snapshot]("SPX-2024-01-31.json"),
      readJsonResource[IO, Snapshot]("AMZN-2024-01-31.json")
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

      def toPureObs(
          quotes: List[OptionQuote],
          forward: Forward[YearFraction]
      ): List[PureObservation] =
        val filteredQuoted = quotes
          .groupBy(_.expiry)
          .map: (expiryDate, quotes) =>
            val expiry = expiryDate.atTime(16, 0).atZone(stock.expiryZone).toInstant
            val timeToMaturity = TimeLike[java.time.Instant]
              .yearFractionBetween(stock.timestamp.toInstant, expiry)
            val f = forward(timeToMaturity)

            val filteredQuotes = quotes.filter: quote =>
              val otm = (quote.isPut && quote.strike < f) || (quote.isCall && quote.strike > f)
              val delta = (quote.isPut && quote.delta.exists(
                _ < -0.05
              )) || (quote.isCall && quote.delta.exists(_ > 0.05))
              otm && delta

            timeToMaturity -> filteredQuotes

        filteredQuoted
          .filter((t, _) => t.toDouble > 0.01)
          .toList
          .flatMap: (t, quotes) =>
            quotes
              .map: q =>
                val pureStrike = buehler.strikeToPureStrike(
                  q.strike.toDouble,
                  forward(t),
                  forward.dividendFloor(t)
                )
                q.impliedVol
                  .flatMap(_.mid)
                  .map: vol =>
                    PureObservation(pureStrike, t, vol, 0.01)
              .flatten

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
        pureObs = toPureObs(quotes, forward)
        lv <- IO.fromEither(lvFitter.fitPureObservations(pureObs, lvSettings))
        _ <- IO.println(lv)
      yield ()
