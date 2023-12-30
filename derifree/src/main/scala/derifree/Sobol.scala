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
 * Licence pertaining to construction of Sobol numbers and the accompanying
 * sets of direction numbers:
 *
 * Copyright (c) 2008, Frances Y. Kuo and Stephen Joe
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 *     * Neither the names of the copyright holders nor the names of the
 *       University of New South Wales and the University of Waikato
 *       and its contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package derifree

import cats.data.State
import cats.syntax.all.*

import java.io.FileNotFoundException
import scala.collection.View
import scala.collection.immutable.ArraySeq

import Sobol.*

/*
 * https://web.maths.unsw.edu.au/~fkuo/sobol/
 */
sealed trait Sobol:

  def initialState(offset: Int): S

  def next: State[S, IndexedSeq[Double]]

  def skipTo(pos: Int): State[S, Unit]

  def view(offset: Int): View[IndexedSeq[Double]] =
    View.Unfold(initialState(offset)): s =>
      val (nextState, xs) = next.run(s).value
      Some(xs, nextState)

object Sobol:

  enum Error(message: String) extends derifree.Error(message):
    case MaxDimensionsExceeded extends Error("max dimensions exceeded")
    case FailedToParseDirectionNumbers(line: String) extends Error(s"failed to parse: $line")
    case Wrapper(t: Throwable) extends Error(t.getMessage)

  opaque type DirectionNumbers = IndexedSeq[IndexedSeq[Int]]

  object DirectionNumbers:

    private[Sobol] def apply(dirs: IndexedSeq[IndexedSeq[Int]]): DirectionNumbers = dirs

    extension (dirs: DirectionNumbers)
      def get: IndexedSeq[IndexedSeq[Int]] = dirs
      def maxDim: Int = dirs.length

  case class S(pos: Int, x: IndexedSeq[Int])

  def directionNumbers(maxDim: Int): Either[Error, DirectionNumbers] =
    directionNumbersFromResource(maxDim, "new-joe-kuo-6.21201")

  def apply(dim: Int, directionNumbers: DirectionNumbers): Either[Error, Sobol] =
    Either
      .raiseWhen(dim > directionNumbers.length)(Error.MaxDimensionsExceeded)
      .map(_ =>
        new Sobol:

          // skip first dimension: (0,...,0)
          def initialState(offset: Int): S =
            skipTo(offset + 1).runS(S(0, Array.ofDim[Int](dim).toIndexedSeq)).value

          def next: State[S, IndexedSeq[Double]] =
            State[S, IndexedSeq[Double]]: state =>
              val out = Array.ofDim[Double](dim)
              if (state.pos == 0) then
                var i = 0
                while (i < dim) {
                  out(i) = 0.0
                  i += 1
                }
                (state.copy(pos = state.pos + 1), ArraySeq.unsafeWrapArray(out))
              else
                var c = 1
                var value = state.pos - 1
                while ((value & 1) == 1) {
                  value >>= 1
                  c += 1
                }

                var i = 0
                val x = state.x.toArray
                while (i < dim) {
                  x(i) ^= directionNumbers(i)(c)
                  out(i) = x(i) / SCALE
                  i += 1
                }
                (
                  state.copy(pos = state.pos + 1, ArraySeq.unsafeWrapArray(x)),
                  ArraySeq.unsafeWrapArray(out)
                )

          def skipTo(pos: Int): State[S, Unit] =
            require(
              pos >= 0 && pos < MAXPOS,
              s"Position must be between 0 (inclusive) and 2^${BITS} - 1"
            )
            val x = Array.ofDim[Int](dim)
            if (pos == 0) {
              var i = 0
              while (i < dim) {
                x(i) = 0
                i += 1
              }
            } else {
              val i = pos - 1
              val gray = i ^ (i >> 1)
              var j = 0
              while (j < dim) {
                var result = 0
                var k = 1
                var shift = gray >> (k - 1)
                while (shift != 0 && k <= BITS) {
                  val ik = shift & 1
                  result ^= ik * directionNumbers(j)(k)
                  k += 1
                  shift = gray >> (k - 1)
                }
                x(j) = result
                j += 1
              }
            }
            State.set(S(pos, ArraySeq.unsafeWrapArray(x)))
      )

  private val BITS: Int = 31
  private val MAXPOS: Int = ((1L << BITS) - 1).toInt
  private val SCALE: Double = (1L << BITS).toDouble

  private def parseLine(line: String): Option[IndexedSeq[Int]] =
    val tokens = line.split(" ").toList.filter(_.nonEmpty).traverse(_.toIntOption)
    tokens.flatMap:
      case _ :: s :: a :: m_i =>
        val dirs = Array.ofDim[Int](BITS + 2)
        val m = (0 :: m_i).toArray
        var i = 1
        while (i <= s) {
          dirs(i) = m(i) << (BITS - i)
          i += 1
        }
        while (i <= BITS) {
          dirs(i) = dirs(i - s) ^ (dirs(i - s) >> s)
          var k = 1
          while (k < s) {
            dirs(i) ^= ((a >> (s - 1 - k)) & 1) * dirs(i - k)
            k += 1
          }
          i += 1
        }
        dirs.toIndexedSeq.some
      case _ => None

  private def directionNumbersFromResource(
      maxDim: Int,
      resource: String
  ): Either[Error, DirectionNumbers] =
    val d0 =
      val dirs = Array.ofDim[Int](BITS + 2)
      var i = 1
      while (i <= BITS) {
        dirs(i) = 1 << (BITS - i)
        i += 1
      }
      dirs.toIndexedSeq

    Either
      .catchOnly[FileNotFoundException](scala.io.Source.fromResource(resource))
      .leftMap(t => Error.Wrapper(t))
      .flatMap(
        _.getLines
          .drop(1) // drop header
          .take(maxDim)
          .map(line =>
            Either.fromOption(
              parseLine(line),
              Error.FailedToParseDirectionNumbers(s"line: $line")
            )
          )
          .toList
          .sequence
      )
      .map(l => DirectionNumbers((d0 :: l).toArray.toIndexedSeq))
      .ensure(Error.MaxDimensionsExceeded)(_.length >= maxDim)
