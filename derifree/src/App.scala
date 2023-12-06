package app

import cats.effect.*
import cats.syntax.all.*
import derifree.Sobol

object Main extends IOApp.Simple:

  def run: IO[Unit] = Sobol.directionNumbersFromResource[IO](100).flatMap { dirNums =>
    val sobol = Sobol(10, dirNums)
    val seq = sobol.next.replicateA(10).runA(sobol.initialState).value
    IO.println(seq)
  }
