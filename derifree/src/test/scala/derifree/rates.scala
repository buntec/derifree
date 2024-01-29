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

class RatesSuite extends munit.FunSuite:

  // TODO: this is just a skeleton
  test("Nelson-Siegel"):

    val spotRates = List(
      (0.0958904109589041, 0.02848041304570127),
      (0.10410958904109589, 0.024043224463507495),
      (0.10684931506849316, 0.03516212411192165),
      (0.11506849315068493, 0.029016641950306406),
      (0.1232876712328767, 0.04881325330734361),
      (0.13424657534246576, 0.037338321746809476),
      (0.16986301369863013, 0.042971700093358),
      (0.23013698630136986, 0.050992482377662525),
      (0.2602739726027397, 0.04921336607149638),
      (0.30684931506849317, 0.057536172876188396),
      (0.3452054794520548, 0.05261785142142841),
      (0.40273972602739727, 0.05809354353288789),
      (0.42191780821917807, 0.06000649294977801),
      (0.4794520547945205, 0.05280571379580465),
      (0.5561643835616439, 0.05139945779762116),
      (0.6520547945205479, 0.05103066362595948),
      (0.6794520547945205, 0.05040302474167674),
      (0.7287671232876712, 0.050309602615153555),
      (0.8054794520547945, 0.04981851615011721),
      (0.9013698630136986, 0.0484450927907524),
      (0.9315068493150684, 0.049531719312425),
      (0.9780821917808219, 0.0485041799133047),
      (1.073972602739726, 0.04697656473488737),
      (1.1506849315068493, 0.046800785373432074),
      (1.4, 0.045787323342951214),
      (1.8986301369863015, 0.028496471186895064),
      (2.8958904109589043, 0.043603891385213166),
      (3.893150684931507, 0.04006312472693736),
      (4.890410958904109, 0.037117918749320716),
      (5.906849315068493, 0.03958546886655408)
    ).map((t, r) => YearFraction(t) -> Rate(r))

    val params = rates.nelsonsiegel.fitByOlsOnLambdaGrid(spotRates).toTry.get

    assertEqualsDouble(params.beta0, 0.034, 0.01)
    assertEqualsDouble(params.beta1, -0.039, 0.01)
    assertEqualsDouble(params.beta2, 0.125, 0.01)
