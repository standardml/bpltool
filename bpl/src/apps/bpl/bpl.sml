(* Copyright (c) 2006  The BPL Group at the IT University of Copenhagen
 *
 * This file is part of BPL.
 *
 * BPL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * BPL is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with BPL; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA
 *)

(** Declarations that open modules at top level for experimenting with
 * bigraph operations at the interactive command prompt.
 * @version $LastChangedRevision: 315 
 *)

local
  structure ErrorHandler : ERRORHANDLER = PrintErrorHandler
in
  structure BG = BG' (structure ErrorHandler = ErrorHandler)
end

local
  fun getRev s = getOpt (Int.fromString s, 0)
  val op o = General.o
  val revisions
    = [hd (String.tokens (not o Char.isDigit) "$LastChangedRevision$"),
       BG.revision]
in
  val BPL_REVISION_STR
    = Int.toString (foldr Int.max 0 (map getRev revisions))
end

local
fun help' [] = ()
  | help' ("control" :: topics) = (print
  	"Node controls (K,L,M : string, m,n >= 0):\n\
  	\  active   (K -: n)         Active node of global arity n\n\
  	\  passive  (L =: m --> n)   Passive node of local/global arity m/n\n\
  	\  atomic   (L =: m --> n)   Atomic node of local/global arity m/n\n\
  	\  active0  (M)              Active node without ports\n\
  	\  passive0 (M)              Passive node without ports\n\
  	\  atomic0  (M)              Atomic node without ports\n";
  	help'' topics)
  | help' ("bigraph" :: topics) = (print
	  "Ions (K,L,M : control, x,y : string):\n\
	  \  K[y,...]                  Ion with control of global arity\n\
	  \  L[y,...][[x,...],...]     Ion with control of global/local arity\n\n\
	  \Wirings (x,y : string):\n\
	  \  y/x                       Renaming link\n\
	  \  y//[x,...]                Substitution link\n\
	  \  y//[]                     Name introduction\n\
	  \  -/x                       Closure edge\n\
	  \  -//[x,...]                Multiple closure edges\n\
	  \  idw[x,...]                Identity wiring\n\n\
	  \Concretions and merges (n >= 0, x : string):\n\
	  \  `[x,...]`                 Concretion of names x,...\n\
	  \  merge(n)                  Merge of inner width n\n\
	  \  <->                       Barren root (= merge 0)\n\n\
	  \Permutations (0 <= i_k < m, x : string):\n\
	  \  @[i_0, ..., i_{m-1}]      Permutation mapping site k to root i_k\n\
	  \  @@[..., i_k&[x,...], ...] Permutation with local names\n\
	  \  idp(m)                    Identity permutation of width m\n\n\
	  \Abstractions, products, compositions (x : string, A,B,P : bgval, n >= 0):\n\
	  \  <[x,...]> P               Abstract names x,... of a prime P\n\
	  \  A * B                     Tensor product\n\
	  \  A || B                    Parallel product\n\
	  \  A `|` B                   Prime product\n\
	  \  **[A,...]                 Tensor procuct of n factors\n\
	  \  |||[A,...]                Parallel product of n factors\n\
	  \  `|``[A,...]               Prime product of n factors\n\
	  \  A o B                     Composition\n";
	  help'' topics)
  | help' ("operator" :: topics) = (print
	  "ML function composition (f : 'b -> c, g : 'a -> 'b):\n\
	  \  f oo g\n\n\
	  \Precedence:\n\
	  \  o                         Composition (strongest)\n\
	  \  *, ||, `|`                Product, left associative\n\
	  \  <[x,...]> P               Abstraction (weakest)\n";
	  help'' topics)
  | help' ("operation" :: topics) = (print
	  "Operations (A,B,R : 'a bgbdnf, a,r,v : bgval, e : exn):\n\
	  \  norm_v v             denorm_b B           (De)normalise\n\
	  \  regl_v v             regl_b B             Regularise\n\
	  \  simpl_v v            simpl_b B            Attempt to simplify\n\
	  \  match_v {agent = a,  match_b {agent = A,  Match redex in agent,\n\
	  \           redex = r}           redex = R}    returning lazy list of matches\n\
	  \  str_v v              str_b B              Return as a string\n\
	  \  print_v v            print_b B            Print to stdOut\n\
	  \  print_mv mz          print_mb mz          Print lazy list of matches\n\
	  \  print_mtv mz         print_mtb mz         Print lazy list of matches with trees\n\
	  \  explain e                                 Explain exception in detail\n";
	  help'' topics)
	| help' ("instantiation" :: topics) = (print
	  "Instantiations:\n\
	  \  @[i_0 |-> j_0, ..., i_m-1 |-> j_m-1]      Instantiation mapping redex\n\
	  \                                              site i_k to reactum site j_k\n\
	  \  @@[..., i_k&[x_0,...,x_m-1] |--> j_k&[y_0,...,y_m-1], ...]\n\
	  \                                            Instantiation mapping local redex\n\
	  \                                              name x_k to reactum name y_k\n";
	  help'' topics)
  | help' ("example" :: topics) = (print
	  "Example:\n\
	  \  let val K = active   (\"K\" =: 2 --> 1)\n\
	  \      val L = passive0 (\"L\")\n\
	  \      val (x,y,z) = (\"x\", \"y\", \"z\")\n\
	  \  in <[y]> (y//[x,z] * merge(2)) o (K[z][[y,z],[]] * id_X[x] * L)\n\
	  \            o (@@[1&[],0&[y,z]] * x//[])\n\
	  \  end handle error => explain error\n";
	  help'' topics)
	| help' _
	= help' ["control", "bigraph", "operator", "operation", "example"]
and help'' [] = ()
  | help'' topics = (print "\n"; help' topics)
fun help''' []
  = (print "  (help \
          	\[\"control\", \"bigraph\", \"operator\",\
          	\ \"operation\", \"example\"]):\n\n";
          help' ["control", "bigraph", "operator", "operation", "example"])
  | help''' topics = (print " ('help []' for all topics):\n\n"; help' topics)
in
fun help topics =
  (print "BPL Usage Help"; help''' topics)
end

open BG.Sugar
infix 3 oo
val op oo = General.o
infix 7 /  infix 7 //
infix 6 o
infix 5 *  infix 5 ||  infix 5 `|`
infix 4 >
infix 3 &  infix 3 --> infix 3 --   infix 3 --|>  infix 3----|>
infix 2 =: infix 2 -:  infix 2 |->  infix 2 |-->  infix 2 :::
nonfix @
nonfix <

open LazyList
type B = BG.BgBDNF.B
type BR = BG.BgBDNF.BR
type 'class bgbdnf = 'class BG.BgBDNF.bgbdnf
type match = BG.Match.match
type rule = BG.Rule.rule
val norm_v = BG.BgBDNF.make
val denorm_b = BG.BgBDNF.unmk
val regl_b = BG.BgBDNF.regularize
fun regl_v v = regl_b (norm_v v)
val simpl_v = BG.BgVal.simplify
fun simpl_b b = simpl_v (denorm_b b) 
val match_rbdnf = BG.Match.matches
fun match_b {agent, redex}
  = let
      val redex = regl_b redex
      val react = denorm_b redex
    in
      match_rbdnf
        {agent = regl_b agent,
         rule = BG.Rule.make'
                  {name = "Rule",
                   redex = redex,
                   react = react}}
    end
fun match_v {agent, redex}
  = let
      val redex = regl_v redex
      val react = simpl_b redex
    in
      match_rbdnf
        {agent = regl_v agent,
         rule = BG.Rule.make'
                  {name = "Rule",
                   redex = redex,
                   react = react}}
    end
fun react_b {agent, rules} =
  case BG.Match.arrmatch {agent = regl_b agent, rules = rules} of
    NONE => NONE
  | SOME m =>
    let
      open BG
      val {context, rule, parameter} = Match.unmk m
      val {react, inst, ...} = Rule.unmk rule
      val instparm = Instantiation.instantiate inst parameter
      val Z = Interface.glob (BgVal.outerface parameter)
      val id_Z = BgVal.Wir Info.noinfo (Wiring.id_X Z)
    in
      SOME (context o (id_Z * react) o instparm)
    end
fun react_v {agent, rules} =
  react_b {agent = norm_v agent, rules = rules}

val str_b = BG.BgBDNF.toString
val str_v = BG.BgVal.toString
fun print_b b = print (str_b b)
fun print_v v = print (str_v v)
fun print_mb mz
  = (LazyList.lzprintln BG.Match.toString mz; print "\n")
fun print_mtb mz
  = (LazyList.lzprintln BG.Match.toStringWithTree mz; print "\n")
local
	fun print_m0 mz toStr =
	  let
	    fun ppBDNF indent pps B
	      = BG.BgVal.pp indent pps (simpl_b B)
	  in
	    LazyList.lzprintln (toStr ppBDNF ppBDNF) mz;
	    print "\n"
	  end
in
  fun print_mv mz = print_m0 mz BG.Match.toString'
  fun print_mtv mz = print_m0 mz BG.Match.toStringWithTree'
end

fun explain e = (BG.ErrorHandler.explain e; raise e);

open BG.Reaction
infixr 4 TIMES_DO
infix  3 ++
infix  2 ORTHEN
infixr 1 THEN
infixr 1 ELSE

fun use_shorthands flag =
 (Flags.setBoolFlag "/kernel/ast/bgterm/ppids" (not flag);
  Flags.setBoolFlag "/kernel/ast/bgterm/pp0abs" (not flag)) 