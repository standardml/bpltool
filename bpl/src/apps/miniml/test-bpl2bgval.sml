open TextIO;

val signat = []
val bigraph = Empty
val dec = Value("id", bigraph)
val prog = Prog(signat,[dec])
val (s,b,r) = prog2bgval prog
val bgval = (B.toString o B.simplify) b

val _ = print bgval
val _ = print "\n"
