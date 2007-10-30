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

datatype 'a expected = HAS of 'a | JUST of 'a

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
  val K10 = active   ("K10" =: 1 --> 0)
  val K11 = active   ("K11" =: 1 --> 1)
  val L0  = passive0 ("L0")
  val L1  = passive  ("L1" -: 1)
  val L2  = passive  ("L2" -: 2)
  val L10 = passive  ("L10" =: 1 --> 0)
  val L11 = passive  ("L11" =: 1 --> 1)
  val M0  = atomic0  ("M0")
  val M1  = atomic   ("M1" -: 1)
  val M2  = atomic   ("M2" -: 2)
  val M10 = atomic   ("M10" =: 1 --> 0)
  val M11 = atomic   ("M11" =: 1 --> 1)

  val (x, y, z, u, v, w) = ("x", "y", "z", "u", "v", "w")
  val (x1, x2, x3) = ("x1", "x2", "x3")
  val (y1, y2, y3) = ("y1", "y2", "y3")
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
    ("Matching empty agent with nonempty redex",
     {agent = -//[],
      redex = K0},
     JUST []),
    ("Matching with id_Z nonempty",
     {agent = (K1[x] * z/z) o L1[z] o <->,
      redex = K1[x]},
     HAS [{context = idp(1) * idw[z,x], parameter = L1[z] o <->}]),
    ("Matching internal agent edge with internal redex edge",
     {agent = -/x o M1[x],
      redex = -/y o M1[y]},
     HAS [{context = idp(1), parameter = idx0}]),
    ("Matching inner bound name with nothing",
     {agent = K10[][[x]] o (<[x]> x//[] o <->),
      redex = K10[][[x]]},
     HAS [{context = idp(1), parameter = (<[x]> x//[] o <->)}]),
    ("Internal redex edge should not match agent name",
     {agent = (x/x * merge(2)) o (K0 o merge(2) o (K0 o <-> * K0 o <->) * K1[x] o K0 o <->),
      redex = (-/x * idp(1)) o K1[x]},
     JUST []),
    ("Internal agent edges ending up in context",
     {agent = (-/x * idp(1)) o M1[x],
      redex = idx0},
     HAS [{context = (-/x * idp(1)) o M1[x], parameter = idx0}]),
    ("Internal agent edges ending up in context, node in parameter",
     {agent = (-/x * idp(1)) o M1[x],
      redex = idp(1)},
     HAS [{context = (-/x * idp(1)), parameter = M1[x]}]),
    ("Matching internal agent edges with redex names with node in context",
     {agent = (-/x * idp(1)) o M1[x] * M0,
      redex = M0},
     HAS [{context = (-/x * idp(2)) o (M1[x] * `[]`), parameter = idx0}]),
    ("Matching internal agent edges with redex names with node in parameter",
     {agent = (-/x o x//[x,y] * idp(1)) o (y/y * K1[x]) o M1[y],
      redex = K1[u]},
     HAS [{context = -/x o x//[u,y] * idp(1), parameter = M1[y]}]),
    ("Matching internal agent edges: siblings with nested linked parameter",
     {agent = (-/x o x//[x,y,z] * merge(2)) o ((z/z * K1[x]) o M1[z] * M1[y]),
      redex = (      w//[u,v]   * merge(2)) o (       K1[u]          * M1[v])},
     HAS [{context = -/x o x//[w,z] * idp(1), parameter = M1[z]}]),
    ("Matching internal agent edges: siblings with nested linked parameter (swapped)",
     {agent = (-/x o x//[x,y,z] * merge(2)) o (M1[y] * (z/z * K1[x]) o M1[z]),
      redex = (      w//[u,v]   * merge(2)) o (M1[v] *        K1[u]         )},
     HAS [{context = -/x o x//[w,z] * idp(1), parameter = M1[z]}]),
    ("Matching internal agent edges: siblings with nested linked parameters",
     {agent = (-/x o x//[x,y] * merge(2)) o ((x/x * K0) o M1[x] * (y/y * K0) o M1[y]),
      redex = (                 merge(2)) o (       K0          *        K0         )},
     HAS [{context = -/x o x//[x,y] * idp(1), parameter = M1[x] * M1[y]}]),
    ("Matching bound links",
     {agent = K10[][[x]] o (<[x]> M1[x]),
      redex = K10[][[y]]},
     JUST [{context = idp(1), parameter = <[y]> M1[y]}]),
    ("Matching redex inner name with nothing",
     {agent = <->, redex = `[x]`},
     HAS [{context = -/x * idp(1), parameter = <[x]> x//[] * <->}]),
    ("Matching redex link with agent edge",
     {agent = (-/x o x//[x1,x2] * idp(1)) o (x2/x2 * K1[x1]) o M1[x2],
      redex =                                        M1[y]},
     HAS
       [{context   = (-/x o x//[x,y] * idp(1)) o (y/y * K1[x]),
         parameter = idx0}]),
    ("Matching wide redex in an agent with internal edge",
     {agent = (-/x o x//[x,x1,x2] * merge(2)) o 
              ((K1[x] * x1/x1) o M1[x1] * K1[x2] o <->),
      redex = (      y//[y1,y2,y3] * idp(2)) o
              (                  M1[y1] * (K1[y2] * y3/y3) o `[y3]`)},
     HAS
       [{context   = (-/x o x//[x,y] * merge(2)) o (K1[x] * y/y * idp(1)),
         parameter = <[y3]> (y3//[] * <->)}]),
    ("Pi calculus reaction rule",
     {agent = (idw[y,z] * K0) o (K1[y] * idw[z]) o M1[z]
          `|` (idw[y]   * K0) o L1[y] o <->,
      redex =  (x/x * K0) o (K1[x] `|` idp(1))
           `|` (x/x * K0) o (L1[x]  `|` idp(1))},
     HAS
       [{context   = y/x * z/z * idp(1),
         parameter = M1[z] * <-> * <-> * <->}]),
    ("Pi calculus reaction rule with nonengaged parts",
     {agent = (idw[y,z] * K0 o merge(2)) o (M0 * K1[y] * idw[z]) o M1[z]
          `|` (idw[y]   * K0 o merge(2)) o (L0 o <-> * L1[y]) o K0 o <->,
      redex =  (x/x * K0) o (K1[x] `|` idp(1))
           `|` (x/x * K0) o (L1[x]  `|` idp(1))},
     HAS [{context   = y/x * z/z * idp(1),
           parameter = M1[z] * M0 * K0 o <-> * L0 o <->}])
  ]
end
  
  fun inst_id redex = Instantiation.make
    {I = BgVal.innerface redex,
     J = BgVal.innerface redex,
     maps = []}
  
  fun mkTest (label, {agent, redex}, matches) =
    let
      fun insidebox f (JUST ms) = JUST (f ms)
        | insidebox f (HAS ms)  = HAS (f ms)
      fun normalise {context, parameter}
        = {context = BgBDNF.make context,
           parameter
             = #D 
                 (BgBDNF.unmkBR
                   (BgBDNF.regularize (BgBDNF.make parameter)))} 
      val matches = insidebox (map normalise) matches
      val rule =
        Rule.make
          {name = "Testrule",
           redex = BgBDNF.regularize
             (BgBDNF.make redex),
           react = redex,
           inst = inst_id redex,
           info = Info.noinfo}
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
      fun listsubset xs ys =
        List.all
         (fn x =>
            case List.find (fn y => match_eq (y, x)) ys of
              SOME _ => true
            | _ => false)
          xs
      fun listfind eq x [] = ([], false)
        | listfind eq x (y :: ys) =
          let
            val (ys', found) = listfind eq x ys
          in
            if eq (x, y) then
              (ys', true)
            else
              (y :: ys', found)
          end
       (* List equality with unique elements in first list. *)
       fun listeq eq [] [] = true
         | listeq eq [] (_ :: _) = false
         | listeq eq (x :: xs) ys =
           let
             val (ys', found) = listfind eq x ys
           in
             found andalso listeq eq xs ys'
           end
    in
      (label,
       fn () =>
         case matches of
           JUST [] =>
            (case lzunmk gotmatches of
               Nil => ()
             | Cons (m, _) =>
               Assert.fail
                 ("expected no matches for a = "
                ^ BgVal.toString agent
                ^ ", R = " ^ BgVal.toString redex
                ^ "\but found\ncontext = "
                ^ BgBDNF.toString (#context (Match.unmk' m))
                ^ "\nparameter = "
                ^ BgBDNF.toString (#parameter (Match.unmk' m))))
         | JUST matches =>
           if listeq match_eq matches (lztolist gotmatches) then
             ()
           else
             Assert.fail
              ("unexpected or missing matches for\na = "
               ^ BgVal.toString agent
               ^ "\nR = " ^ BgVal.toString redex)
         | HAS matches =>
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
