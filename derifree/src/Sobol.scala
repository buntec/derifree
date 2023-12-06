/*-----------------------------------------------------------------------------
 * Licence pertaining to construction of Sobol numbers and the accompanying sets of direction numbers
 *
 * -----------------------------------------------------------------------------
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

import cats.data.{State => cState}
import cats.syntax.all.*
import cats.MonadThrow
import cats.effect.*
import fs2.Stream

import Sobol.*
import fs2.io.file.Files
import fs2.io.file.Path

import scala.collection.immutable.ArraySeq

/*
 * https://web.maths.unsw.edu.au/~fkuo/sobol/
 */
trait Sobol:

  def initialState: State

  def next: cState[State, IndexedSeq[Double]]

  def skipTo(pos: Int): cState[State, Unit]

object Sobol:

  opaque type DirectionNumbers = IndexedSeq[IndexedSeq[Int]]

  object DirectionNumbers:

    private[Sobol] def apply(dirs: IndexedSeq[IndexedSeq[Int]]): DirectionNumbers = dirs

    extension (dirs: DirectionNumbers)
      def get: IndexedSeq[IndexedSeq[Int]] = dirs
      def maxDim: Int = dirs.length

  case class State(pos: Int, x: IndexedSeq[Int])

  def directionNumbersFromResource[F[_]: Concurrent: Sync](maxDim: Int): F[DirectionNumbers] =
    directionNumbersFromByteStream[F](fs2.io.readClassLoaderResource[F]("new-joe-kuo-6.21201"), maxDim)

  def directionNumbersFromFile[F[_]: Concurrent: Files](path: Path, maxDim: Int): F[DirectionNumbers] =
    directionNumbersFromByteStream[F](Files[F].readAll(path), maxDim)

  def apply(dim: Int, directionNumbers: DirectionNumbers): Sobol = new Sobol:

    // skip first dimension: (0,...,0)
    def initialState: State = next.runS(State(0, Array.ofDim[Int](dim).toIndexedSeq)).value

    def next: cState[State, IndexedSeq[Double]] =
      cState[State, IndexedSeq[Double]]: state =>
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
          (state.copy(pos = state.pos + 1, ArraySeq.unsafeWrapArray(x)), ArraySeq.unsafeWrapArray(out))

    def skipTo(pos: Int): cState[State, Unit] =
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
      cState.set(State(pos, ArraySeq.unsafeWrapArray(x)))

  private val BITS: Int = 31
  private val MAXPOS: Int = ((1L << BITS) - 1).toInt
  private val SCALE: Double = (1L << BITS).toDouble

  private def directionNumbersFromByteStream[F[_]: Concurrent](
      bytes: Stream[F, Byte],
      maxDim: Int
  ): F[DirectionNumbers] =

    def parseLine(line: String): Option[IndexedSeq[Int]] =
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

    val d0 =
      val dirs = Array.ofDim[Int](BITS + 2)
      var i = 1
      while (i <= BITS) {
        dirs(i) = 1 << (BITS - i)
        i += 1
      }
      dirs.toIndexedSeq

    val lines =
      bytes
        .through(fs2.text.utf8.decode)
        .through(fs2.text.lines)
        .drop(1) // drop header
        .take(maxDim)

    (fs2.Stream(d0).covary[F] ++ lines.evalMap: line =>
      MonadThrow[F].fromOption(parseLine(line), new Exception(s"failed to parse line: $line"))).compile.toList
      .map(_.toArray.toIndexedSeq)
      .map(DirectionNumbers(_))
      .ensure(new IllegalArgumentException("found fewer than the requested number of dimensions"))(_.length >= maxDim)
