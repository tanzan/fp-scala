val rna = RNA(A, T, G, A)

rna.take(3)

rna.map(x => x)

rna.flatMap(x => RNA(x))

rna.map(_.toString)