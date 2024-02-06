package derifree
package fd

import derifree.fd.LocalVolFitter.*

import scala.math.sqrt

class LVFitSuite extends munit.FunSuite:

  test("black-scholes with term-structure: fitted local-vol should match forward vol"):

    val fitter = LocalVolFitter.apply

    val expiries =
      List(YearFraction.oneYear / 12, YearFraction.oneYear / 4, YearFraction.oneYear)

    val vols = List(0.3, 0.35, 0.4)

    val forwardVols = vols.head :: vols
      .zip(vols.tail)
      .zip(expiries)
      .zip(expiries.tail)
      .map:
        case (((v1, v2), t1), t2) =>
          sqrt((v2 * v2 * t2.toDouble - v1 * v1 * t1.toDouble) / (t2.toDouble - t1.toDouble))

    val obs = expiries
      .zip(vols)
      .flatMap((t, vol) =>
        List(
          PureObservation(0.90, t, vol, 0.01),
          PureObservation(0.95, t, vol, 0.01),
          PureObservation(0.99, t, vol, 0.01),
          PureObservation(1.00, t, vol, 0.01),
          PureObservation(1.01, t, vol, 0.01),
          PureObservation(1.05, t, vol, 0.01),
          PureObservation(1.10, t, vol, 0.01)
        )
      )
    val settings = Settings(3, 0.01, 1.0)
    val result = fitter.fitPureObservations(obs, settings).toTry.get

    val clue =
      s"grid bounds=${result.gridBounds} expiries=${result.expiries}, knots=${result.lvKnots}, lvs=${result.lvAtKnots}"

    result.lvAtKnots
      .zip(forwardVols)
      .foreach: (lvs, fwdVol) =>
        lvs.foreach(lv => assertEqualsDouble(lv, fwdVol, 0.0001, clue))

    val surface = fitter.pureVolSurface(result).toTry.get

    obs.foreach: obs =>
      val vol = surface(obs.expiry, obs.strike).get
      val clue = s"vol=$vol, refVol=${obs.vol}, strike=${obs.strike}, expiry=${obs.expiry}"
      assertEqualsDouble(vol, obs.vol, 0.0001, clue)
