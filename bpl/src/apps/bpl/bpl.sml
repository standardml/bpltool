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
  structure BG = BG (structure ErrorHandler = ErrorHandler)
end

fun help _ = print
  "BPL Usage Help:\n\n\
  \Node controls (K,L,M : string, m,n >= 0):\n\
  \  active   (K -: n)         Active node of global arity n\n\
  \  passive  (L =: m --> n)   Passive node of local/global arity m/n\n\
  \  atomic   (L =: m --> n)   Atomic node of local/global arity m/n\n\
  \  active0  (M)              Active node without ports\n\
  \  passive0 (M)              Passive node without ports\n\
  \  atomic0  (M)              Atomic node without ports\n\n\
  \Ions (K,L,M : control, x,y : string):\n\
  \  K[y,...]                  Ion with control of global arity\n\
  \  L[y,...][[x,...],...]     Ion with control of global/local arity\n\n\
  \Wirings (x,y : string):\n\
  \  y/x                       Renaming link\n\
  \  y//[x,...]                Substitution link\n\
  \  y//[]                     Name introduction\n\
  \  -/x                       Closure edge\n\
  \  -//[x,...]                Multiple closure edges\n\
  \  id_X[x,...]               Identity wiring\n\n\
  \Concretions and merges (n >= 0, x : string):\n\
  \  `[x,...]`                 Concretion of names x,...\n\
  \  merge(n)                  Merge of inner width n\n\
  \  <->                       Barren root (= merge 0)\n\n\
  \Permutations (0 <= i_k < m, x : string):\n\
  \  @[i_0, ..., i_{m-1}]      Permutation mapping site k to root i_k\n\
  \  @@[..., i_k&[x,...], ...] Permutation with local names\n\
  \  id_n(m)                   Identity permutation of width m\n\
  \Abstractions, products, compositions (x : string, A,B,P : bgval, n >= 0):\n\
  \  <[x,...]> P               Abstract names x,... of a prime P\n\
  \  A * B                     Tensor product\n\
  \  A || B                    Parallel product\n\
  \  A <|> B                   Prime product\n\
  \  **[A,...]                 Tensor procuct of n factors\n\
  \  |||[A,...]                Parallel product of n factors\n\
  \  <|>>[A,...]               Prime product of n factors\n\
  \  A o B                     Composition\n\n\
  \Precedence:\n\
  \  o                         Composition (strongest)\n\
  \  *, ||, <|>                Product, left associative\n\
  \  <[x,...]> P               Abstraction (weakest)\n\n\
  \Operations (B : 'a bgbdnf, a,R : bgval, e : exn):\n\
  \  toBDNF v                  Normalise v\n\
  \  toRBDNF v                 Normalise and regularise v\n\
  \  regularize B              Regularise B\n\
  \  matches {agent = a,\n\
  \           redex = R}       Match R in a, returning a lazy list of matches\n\
  \  simplify v                Attempt to simplify v\n\
  \  valToString v             Return v as a string\n\
  \  bdnfToString B            Return B as a string\n\
  \  printVal v                Print v\n\
  \  printBDNF B               Print B\n\
  \  printMatches              Print a lazy list of matches\n\
  \  printMatches'             Print a lazy list of matches simplified\n\
  \  explain e                 Explain exception e in detail\n\
  \Example:\n\
  \  let val K = active   (\"K\" =: 2 --> 1)\n\
  \      val L = passive0 (\"L\")\n\
  \      val (x,y,z) = (\"x\", \"y\", \"z\")\n\
  \  in <[y]> (y//[x,z] * merge(2)) o (K[z][[y,z],[]] * id_X[x] * L)\n\
  \            o (@@[1&[],0&[y,z]] * x//[])\n\
  \  end handle error => explain error\n\
  \"
  
open BG.Sugar
infix 7 /   infix 7 //
infix 6 o
infix 5 *   infix 5 ||   infix 5 <|>
infix 4 >
infix 3 &   infix 3 -->
infix 2 =:  infix 2 -:
nonfix @
nonfix <

open LazyList
type B = BG.BgBDNF.B
type BR = BG.BgBDNF.BR
type 'class bgbdnf = 'class BG.BgBDNF.bgbdnf
type match = BG.Match.match
val toBDNF = BG.BgBDNF.make
val regularize = BG.BgBDNF.regularize
fun toRBDNF v = regularize (toBDNF v)
fun matches {agent, redex}
  = BG.Match.matches {agent = toRBDNF agent, redex = toRBDNF redex}
val simplify = BG.BgVal.simplify
val bdnfToString = BG.BgBDNF.toString
val valToString = BG.BgVal.toString
fun printBDNF b = print (bdnfToString b)
fun printVal v = print (valToString v)
fun printMatches mz
  = (LazyList.lzprint BG.Match.toString mz; print "\n")
fun printMatches' mz =
  let
    fun ppBDNF indent pps B
      = BG.BgVal.pp indent pps (simplify (BG.BgBDNF.unmk B))
  in
    LazyList.lzprint (BG.Match.toString' ppBDNF ppBDNF) mz;
    print "\n"
  end
fun explain e = (BG.ErrorHandler.explain e; raise e);
