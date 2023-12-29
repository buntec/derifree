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

class NormalSuite extends munit.FunSuite:

  test("N^{-1}(N(x)) = x"):
    val gen = Gen.between(-10.0, 10.0)
    gen.view
      .map: x =>
        (x, normal.cdf(x))
      .filter((_, p) => p > 0 && p < 1)
      .take(100000)
      .foreach: (x, p) =>
        val x0 = normal.inverseCdf(p)
        assert(math.abs(x - x0) < math.max(1e-7, 1e-6 * x), s"x=$x, p=$p, x0=$x0")

  test("N(N^{-1}(p)) = p"):
    val gen = Gen.between(0.0, 1.0)
    gen.view
      .filter(_ > 0)
      .take(100000)
      .foreach: p =>
        val x = normal.inverseCdf(p)
        val p0 = normal.cdf(x)
        assert(math.abs(p - p0) < 1e-7, s"x=$x, p=$p, p0=$p0")
