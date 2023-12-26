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

class LsmSuite extends munit.FunSuite:

  test("speed of polynomial regression".ignore):
    val rows = List(
      (List(0.1, 2.1), 1.1),
      (List(0.3, 1.1), 0.3),
      (List(0.1, -0.3), 1.3),
      (List(3.1, 1.2), -3.1),
      (List(2.1, 4.2), 3.1),
      (List(3.4, -2.2), 3.7),
      (List(8.2, 1.5), 1.7),
      (List(3.1, -1.8), 2.9),
      (List(7.1, 3.2), 2.9)
    )
    var j = 0
    while (j < 1000) {
      val t1 = System.nanoTime()
      var i = 0
      var sum = 0.0
      while (i < 64000) {
        val lsm = Lsm.fromPoly(3)
        val est = lsm.continuationValueEstimator(rows).toOption.get
        sum += est(IndexedSeq(0.4, 0.2))
        i += 1
      }
      val t2 = System.nanoTime()
      println(s"took ${(t2 - t1) * 1e-6} ms")
      j += 1
    }
