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
