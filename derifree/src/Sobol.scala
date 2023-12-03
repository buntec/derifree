package derifree

import cats.data.{State => cState}
import cats.syntax.all.*
import cats.MonadThrow
import cats.effect.Sync

import Sobol.*

trait Sobol:

  def initialState: State

  def next: cState[State, IndexedSeq[Double]]

  def skipTo(pos: Int): cState[State, Unit]

object Sobol:

  case class State(pos: Int, x: IndexedSeq[Int])

  def apply[F[_]: Sync](dim: Int): F[Sobol] =
    directionNumbersFromResource[F](dim).map(apply(dim, _))

  def apply(dim: Int, directionNumbers: IndexedSeq[IndexedSeq[Int]]): Sobol = new Sobol:

    def initialState: State = State(0, Array.ofDim[Int](dim).toIndexedSeq)

    val next: cState[State, IndexedSeq[Double]] =
      cState[State, IndexedSeq[Double]]: state =>
        val out = Array.ofDim[Double](dim)
        if (state.pos == 0) then
          var i = 0
          while (i < dim) {
            out(i) = 0.0
            i += 1
          }
          (state.copy(pos = state.pos + 1), IArray.unsafeFromArray(out))
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
          (state.copy(pos = state.pos + 1, IArray.unsafeFromArray(x)), IArray.unsafeFromArray(out))

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
      cState.set(State(pos, IArray.unsafeFromArray(x)))

  private val BITS = 31
  private val MAXDIM = 21201
  private val MAXPOS = math.pow(2, BITS).toInt - 1
  private val SCALE = math.pow(2, BITS)

  private def directionNumbersFromResource[F[_]: Sync](dim: Int): F[IndexedSeq[IndexedSeq[Int]]] =

    def parseLine(line: String): Option[IndexedSeq[Int]] =
      val tokens = line.split(" ").toList.filter(_.nonEmpty).traverse(_.toIntOption)
      tokens.flatMap:
        case s :: a :: m_i =>
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

    val lines = fs2.io
      .readClassLoaderResource[F]("/new-joe-kuo-6.21201")
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .drop(1) // drop header
      .take(dim)

    MonadThrow[F].raiseWhen(dim > MAXDIM)(new IllegalArgumentException(s"dim must not exceed $MAXDIM but was $dim")) *>
      (fs2.Stream(d0).covary[F] ++ lines.evalMap: line =>
        MonadThrow[F].fromOption(parseLine(line), new Exception(s"failed to parse line: $line"))).compile.toList
        .map(_.toArray.toIndexedSeq)
