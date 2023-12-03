package app

import cats.effect.*
import cats.syntax.all.*
import derifree.Sobol

object Main extends IOApp.Simple:

  def run: IO[Unit] = Sobol[IO](100).flatMap { sobol =>
    val seq = sobol.next.replicateA(10).runA(sobol.initialState).value
    IO.println(seq)
  }
