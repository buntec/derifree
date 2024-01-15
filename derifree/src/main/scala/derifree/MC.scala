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

trait MC[A, T]:

  def contingentClaim(a: A, refTime: T, dsl: Dsl[T]): dsl.ContingentClaim

object MC:

  def apply[A, T](using ev: MC[A, T]): MC[A, T] = ev

  trait Syntax:

    extension [T, A: MC[_, T]](a: A)
      def contingentClaim(refTime: T, dsl: Dsl[T]): dsl.ContingentClaim =
        MC[A, T].contingentClaim(a, refTime, dsl)
