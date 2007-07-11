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
     {agent=(x/x * merge(2)) o (K0 o merge(2) o (K0 o <-> * K0 o <->) * K1[x] o K0 o <->),
      redex=(-/x * idp(1)) o K1[x]},
     [])
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
      fun lzsubset eq xs yz =
        let
          fun lzss [] _ _ _ = true
            | lzss (x :: xs) (y :: ys) zs yz
            = if eq (x, y) then
                lzss xs zs zs yz
              else
                lzss (x :: xs) ys zs yz
            | lzss (x :: xs) [] zs yz
            = (case lzunmk yz of
                 Nil => false
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
           | _ =>
             Assert.fail
               ("expected no matches for a = "
              ^ BgVal.toString agent
              ^ ", R = " ^ BgVal.toString redex)
         else
           if lzsubset match_eq matches gotmatches then
             ()
           else
             Assert.fail 
               ("expected matches not found for a = "
                ^ BgVal.toString agent
                ^ ", R = " ^ BgVal.toString redex))
    end

  val suite = (fn () => Test.labelTests (map mkTest tests))
end
