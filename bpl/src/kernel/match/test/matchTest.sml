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

(** Testing module for bdnf stuff.
 * @version $LastChangedRevision$
 *)

functor MatchTest (
  structure Assert : ASSERT
	structure Test   : TEST
	structure ErrorHandler : ERRORHANDLER
	  where type ppstream    = PrettyPrint.ppstream
      and type break_style = PrettyPrint.break_style
      and type origin      = Origin.origin) =
struct

open LazyList
structure BGADT :> BG_ADT = BGADT (structure ErrorHandler = ErrorHandler)
              
open BGADT
open Assert

local
  open Sugar
  infix 7 /   infix 7 //
  infix 6 o
  infix 5 *   infix 5 ||   infix 5 `|`
  infix 4 >
  infix 3 &   infix 3 -->
  infix 2 =:  infix 2 -:   infix 2 |->   infix 2 |-->
  nonfix @
  nonfix <

  val K0  = active0  ("K0")
  val K1  = active   ("K1" -: 1)
  val K2  = active   ("K2" -: 2)
  val K11 = active   ("K11" =: 1 --> 1)
  val L0  = passive0 ("L0")
  val L1  = passive  ("L1" -: 1)
  val L2  = passive  ("L2" -: 2)
  val L11 = passive  ("L11" =: 1 --> 1)
  val M0  = atomic0  ("M0")
  val M1  = atomic   ("M1" -: 1)
  val M2  = atomic   ("M2" -: 2)
  val M11 = atomic   ("M11" =: 1 --> 1)

  val (x, y, z, u, w) = ("x", "y", "z", "u", "w")
  val (y_4,y_5,x_6,x_7) = ("y_4","y_5","x_6","x_7")
in
  (** The list of tests.  Each entry consists of:
   * A label describing the test,
   * Agent and redex,
   * A list of matches.
   * 
   * An attempt to match the redex in the agent will
   * be performed, and all the given matches must appear
   * in the resulting list of matches for the test to
   * succeed.
   *)
  val tests = [
    ("Matching with id_Z nonempty",
     {agent = (K1[x] * z/z) o L1[z] o <->,
      redex = K1[x]},
     [{context   = idp(1) * idw[z,x],
       parameter = L1[z] o <->}]),
    ("Internal redex edge should not match agent name",
     {agent = (x/x * merge(2)) o (K0 o merge(2) o (K0 o <-> * K0 o <->) * K1[x] o K0 o <->),
      redex = (-/x * idp(1)) o K1[x]},
     []),
    ("Internal agent edges ending up in context",
     {agent = (-/x * idp(1)) o M1[x],
      redex = idx0},
     [{context = (-/x * idp(1)) o M1[x],
       parameter = idx0}]),
    ("Internal agent edges ending up in context, node in parameter",
     {agent = (-/x * idp(1)) o M1[x],
      redex = idp(1)},
     [{context = (-/x * idp(1)),
       parameter = M1[x]}]),
    ("Matching internal agent edges with redex names with node in context",
     {agent = (-/x * idp(1)) o M1[x] * M0,
      redex = M0},
     [{context = (-/x * idp(2)) o (M1[x] * `[]`),
       parameter = idx0}]),
    ("Pi calculus reaction rule",
     {agent = (idw[y,z] * K0) o (K1[y] * idw[z]) o M1[z]
          `|` (idw[y]   * K0) o L1[y] o <->,
      redex =  (x/x * K0) o (K1[x] `|` idp(1))
           `|` (x/x * K0) o (L1[x]  `|` idp(1))},
     [{context   = y/x * z/z * idp(1),
       parameter = M1[z] * <-> * <-> * <->}]),
    ("Pi calculus reaction rule with nonengaged parts",
     {agent = (idw[y,z] * K0 o merge(2)) o (M0 * K1[y] * idw[z]) o M1[z]
          `|` (idw[y]   * K0 o merge(2)) o (L0 o <-> * L1[y]) o K0 o <->,
      redex =  (x/x * K0) o (K1[x] `|` idp(1))
           `|` (x/x * K0) o (L1[x]  `|` idp(1))},
     [{context   = y/x * z/z * idp(1),
       parameter = M1[z] * M0 * K0 o <-> * L0 o <->}])
  ]
end
  
  fun inst_id redex = Instantiation.make
    {I = BgVal.innerface redex,
     J = BgVal.innerface redex,
     maps = []}
  
  fun mkTest (label, {agent, redex}, matches) =
    let
      fun normalise {context, parameter}
        = {context = BgBDNF.make context,
           parameter
             = #D 
                 (BgBDNF.unmkBR
                   (BgBDNF.regularize (BgBDNF.make parameter)))} 
      val matches = map normalise matches
      val rule =
        Rule.make
          {name = "Testrule",
           redex = BgBDNF.regularize
             (BgBDNF.make redex),
           react = redex,
           inst = inst_id redex}
      val gotmatches = Match.matches
        {agent = BgBDNF.regularize (BgBDNF.make agent), rule = rule}
      (* lzsubset eq xs yz checks whether xs is a subset of yz, using
       * eq comparison.  It returns the boolean result, elements of
       * yz that were evaluated, and the remaining elements of yz.
       *)
      fun lzsubset eq xs yz =
        let
          (* lzss xs ys zs yz tests whether elements of xs are in
           * ys, or, if not, they are in yz.  Elements of yz are
           * only evaluated once, then put into zs.
           *)
          fun lzss [] _ zs yz = (true, zs, yz)
            | lzss (x :: xs) (y :: ys) zs yz
            = if eq (x, y) then
                lzss xs zs zs yz
              else
                lzss (x :: xs) ys zs yz
            | lzss (x :: xs) [] zs yz
            = (case lzunmk yz of
                 Nil => (false, zs, yz)
               | Cons (y, yz') =>
                 if eq (x, y) then
                   lzss xs (y :: zs) (y :: zs) yz'
                 else
                   lzss (x :: xs) [] (y :: zs) yz')
        in
          lzss xs [] [] yz
        end
      fun match_eq ({context, parameter}, m) =
        let
          val {context = context_m, parameter = parameter_m, ...}
            = Match.unmk m
          val result =
            BgBDNF.eq' context context_m andalso
            BgBDNF.eq' parameter parameter_m
          (*val ctx = BgVal.toString_unchanged (BgBDNF.unmk context)
          val ctx_m = BgVal.toString_unchanged (BgBDNF.unmk context_m)
          val par = BgVal.toString_unchanged (BgBDNF.unmk parameter)
          val par_m = BgVal.toString_unchanged (BgBDNF.unmk parameter_m)*)
        in
          (*(if result then
             print (ctx ^ " =\n" ^ ctx_m ^ " &&\n" ^ par ^ " =\n" ^ par_m ^ "\n")
           else
             print (ctx ^ " <>\n" ^ ctx_m ^ " ||\n" ^ par ^ " <>\n" ^ par_m ^ "\n"));*)
          result
        end
    in
      (label,
       fn () =>
         if null matches then
           case lzunmk gotmatches of
             Nil => ()
           | Cons (m, _) =>
             Assert.fail
               ("expected no matches for a = "
              ^ BgVal.toString agent
              ^ ", R = " ^ BgVal.toString redex
              ^ "\but found\ncontext = "
              ^ BgBDNF.toString (#context (Match.unmk' m))
              ^ "\nparameter = "
              ^ BgBDNF.toString (#parameter (Match.unmk' m)))
         else
           let
             val (allmatchesfound, ms, mz)
               = lzsubset match_eq matches gotmatches
           in
             if allmatchesfound then
               ()
             else
               Assert.fail 
                 ("expected matches not among "
                  ^ Int.toString (length ms) ^ " found matches for\na = "
                  ^ BgVal.toString agent
                  ^ "\nR = " ^ BgVal.toString redex)
           end)
    end

  val suite = (fn () => Test.labelTests (map mkTest tests))
end
