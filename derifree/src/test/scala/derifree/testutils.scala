package derifree

import cats.effect.*
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.parser.*

object testutils:

  def readJsonResource[F[_], A: Decoder](
      fileName: String
  )(using F: Async[F]): F[A] =
    fs2.io
      .readClassLoaderResource[F](fileName)
      .through(fs2.text.utf8.decode)
      .compile
      .foldMonoid
      .flatMap(s => F.fromEither(decode[A](s)))
