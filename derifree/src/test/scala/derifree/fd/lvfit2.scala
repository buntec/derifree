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
        snapshot = stock.copy(quotes = quotes)
        lv <- IO.fromEither(
          lvFitter.fitSnapshot(snapshot, forward, refTime, lvSettings, YearFraction.oneDay * 7)
        )
        _ <- IO.println(lv)
      yield ()
