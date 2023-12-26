package derifree.prettyprint

import cats.Show

given [A: Show]: Show[IndexedSeq[IndexedSeq[A]]] =
  Show.show(_.map(_.map(Show[A].show).mkString("[", ", ", "]")).mkString("\n"))

given [A: Show]: Show[Array[Array[A]]] =
  Show.show(_.map(_.map(Show[A].show).mkString("[", ", ", "]")).mkString("\n"))
