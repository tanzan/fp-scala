import core._

val rna = RNA(core.A, T, G, core.A)

rna.take(3)

rna.map(x => x)

rna.flatMap(x => RNA(x))

rna.map(_.toString)