package derifree

import cats.*
import cats.derived.*

case class FairValueResult[T](
    fairValue: PV,
    callProbabilities: Map[T, Double],
    putProbabilities: Map[T, Double]
) derives Show,
      Eq
