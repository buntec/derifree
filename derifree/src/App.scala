package app

import cats.effect.*
import cats.syntax.all.*
import derifree.*
import derifree.prettyprint.given

object Main extends IOApp.Simple:

  def run: IO[Unit] = Sobol
    .directionNumbersFromResource[IO](100)
    .flatMap: dirNums =>
      IO.fromEither(NormalGen.fromSobol(10, 3, dirNums))
    .flatMap: normalGen =>
      val seq = normalGen.next.replicateA(10).runA(normalGen.init).value
      IO.println(seq.map(_.show).mkString("\n\n"))
