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

import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*

class NormalSuite extends munit.ScalaCheckSuite:

  property("N^{-1}(N(x)) = x"):
    given Arbitrary[Double] = Arbitrary(Gen.choose[Double](-5, 5))
    forAll: (x: Double) =>
      val x0 = normal.inverseCdf(normal.cdf(x))
      math.abs(x - x0) < 1e-7

  property("N(N^{-1}(p)) = p"):
    given Arbitrary[Double] = Arbitrary(Gen.choose[Double](0, 1))
    forAll: (p: Double) =>
      val p0 = normal.cdf(normal.inverseCdf(p))
      math.abs(p - p0) < 1e-7
