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

/*
 * NOTE: The following copyright notice applies to the original C++ code,
 *
 * ===========================================================================
 *
 * Copyright (C) 2002 Peter Jäckel "Monte Carlo Methods in Finance".
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software is freely
 * granted, provided that this notice is preserved.
 * ===========================================================================
 */

package derifree

import org.apache.commons.math3.util.{FastMath => math}

private[derifree] object BrownianBridge:

  /** Transforms an array of standard normals into another array of standard normals. For
    * performance reasons this is done in-place and without allocations.
    */
  def transform(numberOfSteps: Int): Array[Double] => Unit =

    val leftIndex, rightIndex, bridgeIndex = Array.fill(numberOfSteps)(0)
    val leftWeight, rightWeight, stddev = Array.fill(numberOfSteps)(0.0)
    val map = Array.fill(numberOfSteps)(0)
    val path = Array.fill(numberOfSteps)(0.0)

    map(numberOfSteps - 1) = 1
    stddev(0) = math.sqrt(numberOfSteps.toDouble)
    bridgeIndex(0) = numberOfSteps - 1
    var j = 0
    var i = 1
    while (i < numberOfSteps) {
      while (map(j) != 0) {
        j += 1
      }
      var k = j
      while (map(k) == 0) {
        k += 1
      }
      val l = j + ((k - 1 - j) >>> 1)
      map(l) = i
      bridgeIndex(i) = l
      leftIndex(i) = j
      rightIndex(i) = k
      leftWeight(i) = (k - l) / (k + 1.0 - j)
      rightWeight(i) = (l + 1.0 - j) / (k + 1.0 - j)
      stddev(i) = math.sqrt((l + 1.0 - j) * (k - l) / (k + 1.0 - j))
      j = k + 1
      if (j >= numberOfSteps) {
        j = 0
      }
      i += 1
    }

    def buildPath(standardNormals: Array[Double]): Array[Double] = {
      path(numberOfSteps - 1) = stddev(0) * standardNormals(0)
      var i = 1
      while (i < numberOfSteps) {
        val j = leftIndex(i)
        val k = rightIndex(i)
        val l = bridgeIndex(i)
        if (j != 0) {
          path(l) = leftWeight(i) * path(j - 1) + rightWeight(i) * path(
            k
          ) + stddev(i) * standardNormals(i)
        } else {
          path(l) = rightWeight(i) * path(k) + stddev(i) * standardNormals(i)
        }
        i += 1
      }
      path
    }

    (standardNormals: Array[Double]) => {
      require(standardNormals.length == numberOfSteps)
      val path = buildPath(standardNormals)
      standardNormals(0) = path(0)
      var i = 1
      while (i < path.length) {
        standardNormals(i) = path(i) - path(i - 1)
        i += 1
      }
    }
