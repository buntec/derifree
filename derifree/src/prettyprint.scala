package derifree.prettyprint

import cats.Show

given [A: Show]: Show[IndexedSeq[IndexedSeq[A]]] =
  Show.show(_.map(_.map(Show[A].show).mkString("[", ", ", "]")).mkString("\n"))
