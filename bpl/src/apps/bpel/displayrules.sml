fun maps2LaTeX maps =
  let
    fun nameseq [] [] = true
      | nameseq (n :: ns) (m :: ms) = BG.Name.== (n, m) andalso nameseq ns ms
      | nameseq _ _ = false
    fun nameseq [_] [_] = true
      | nameseq ns ms = nameseq' ns ms
    fun eq ((j, [_]), (i, [_])) = j = i
      | eq ((j, ns), (i, ms)) = j = i andalso nameseq ns ms
    fun nstoLaTeX [] = ""
      | nstoLaTeX [n] = BG.Name.ekam n
      | nstoLaTeX (n :: ns) = BG.Name.ekam n ^ ", " ^ nstoLaTeX ns
    fun mptoLaTeX ((j, []), (i, _))
      = Int.toString j ^ "\\mapsto " ^ Int.toString i
      | mptoLaTeX ((j, [_]), (i, _))
      = Int.toString j ^ "\\mapsto " ^ Int.toString i
      | mptoLaTeX ((j, ns), (i, ms))
      = Int.toString j ^ "[" ^ nstoLaTeX ns ^ "]\\mapsto " ^
        Int.toString i ^ "[" ^ nstoLaTeX ms ^ "]"
    fun mpstoLaTeX [] = ""
      | mpstoLaTeX [mp] = mptoLaTeX mp
      | mpstoLaTeX (mp :: mps) = mptoLaTeX mp ^ ", " ^ mpstoLaTeX mps
  in
    mpstoLaTeX (List.filter (fn mp => not (eq mp)) maps)
  end
fun rule2LaTeX (rule, s) =
  let
    open BG.PPSVG
    val tosvgstr = ppsvg "" "svg:" NONE NONE
    val {name, redex, react, inst, info} = BG.Rule.unmk rule
    val redexsvg = makesvg NONE (BG.BgBDNF.make (BG.BgBDNF.unmk redex))
    val reactsvg = makesvg NONE (BG.BgBDNF.make react)
    val {I, J, maps} = BG.Instantiation.unmk inst
    val str2LaTeX
      = String.translate
        (fn #" " => "\\ " | #"_" => "\\_" | c => String.str c)
  in
    svgToTikZ 0.02 redexsvg
    (" \\reacts{" ^ str2LaTeX name ^ "}{" ^ maps2LaTeX maps ^ "} " ^
     svgToTikZ 0.02 reactsvg ("\n\n" ^ s))
  end
fun displayrules rules = foldr rule2LaTeX "" rules
fun rules2TeXfile filename rules =
  let
    val file = TextIO.openOut filename
    val str = "\
    \\\documentclass{article}\n\
    \\\usepackage{tikz}\n\
    \\\def\\reacts#1#2{\\ensuremath{\\mathop{\\longrightarrow}\\limits^{#1}_{#2}}}\n\
    \\\begin{document}\\lineskip5mm\\raggedright\n\
    \" ^ 
    displayrules rules ^ "\
    \\\end{document}\n\
    \"
  in
    TextIO.output (file, str);
    TextIO.closeOut file
  end
    