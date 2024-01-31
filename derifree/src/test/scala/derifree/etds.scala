package derifree

import cats.Show
import cats.effect.IO
import cats.syntax.all.*
import derifree.dtos.etd.options.OptionQuote
import derifree.dtos.etd.options.Snapshot
import derifree.syntax.*
import derifree.testutils.*

import scala.concurrent.duration.*

class EtdsSuite extends munit.CatsEffectSuite:

  override def munitIOTimeout: Duration = 5.minutes

  test("fit USD discount curve from SPX snapshot"):
    readJsonResource[IO, Snapshot]("SPX-2024-01-29.json")
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
      readJsonResource[IO, Snapshot]("SPX-2024-01-29.json"),
      readJsonResource[IO, Snapshot]("AMZN-2024-01-29.json")
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

  test("fit borrow curve from option market snapshot".only):
    (
      readJsonResource[IO, Snapshot]("SPX-2024-01-29.json"),
      readJsonResource[IO, Snapshot]("AMZN-2024-01-29.json")
    ).flatMapN: (spx, amzn) =>
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
          (amzn.underlying.bid, amzn.underlying.ask).mapN((bid, ask) => (bid + ask) / 2)
        )(Exception("missing spot"))
        discount <- IO.fromEither(discountFitter.fromSnapshot(spx, refTime))
        forward = Forward[YearFraction](spot.toDouble, divs, discount, initialBorrow)
        borrow <- IO.fromEither(borrowFitter.fromSnapshot(amzn, forward, discount, refTime))
        _ <- List(1, 7, 30, 90, 180, 365, 2 * 365, 3 * 365).traverse_(d =>
          val r = borrow.spotRate(refTime.plusDays(d))
          IO.println(s"d=$d, r=$r")
        )
      yield ()
