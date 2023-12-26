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

import org.apache.commons.math3.random.SobolSequenceGenerator

class SobolSuite extends munit.FunSuite:

  test("numbers match Apache Commons Math implementation"):
    for (dim <- List(5, 10, 50, 100, 500, 1000)) {
      val dirNums = Sobol.directionNumbers(dim).toTry.get
      val sobol = Sobol(dim, dirNums).toTry.get
      val iter = Iterator.unfold(sobol.initialState(0))(state =>
        val (nextState, numbers) = sobol.next.run(state).value
        Some((numbers, nextState))
      )

      val refGen =
        new SobolSequenceGenerator(dim, getClass.getResourceAsStream("/new-joe-kuo-6.21201"))
      refGen.nextVector() // skip all-zero dimension

      iter
        .take(10000)
        .foreach: numbers =>
          val ref = refGen.nextVector().toIndexedSeq
          assertEquals(numbers, ref)
    }
