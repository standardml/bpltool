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

(** Syntactic sugar for creating bgvals in SML.
 * @version $LastChangedRevision$
 *)
functor Sugar'(structure Info : INFO 
	       structure Name : NAME
	       structure NameSet : MONO_SET
	       structure Interface : INTERFACE
	       structure Link : LINK
	       structure LinkSet : MONO_SET
	       structure Control : CONTROL
	       structure Ion : ION
	       structure Wiring : WIRING
	       structure Permutation : PERMUTATION
	       structure BgVal : BGVAL
	       structure BgBDNF : BGBDNF
	       structure Rule : RULE
	       structure Instantiation : INSTANTIATION
	       structure ErrorHandler : ERRORHANDLER
                 where type ppstream    = PrettyPrint.ppstream
                   and type break_style = PrettyPrint.break_style
                   and type origin      = Origin.origin
	       structure NameSetPP : COLLECTIONPRETTYPRINT
                 where type ppstream = PrettyPrint.ppstream
	       sharing type Info.info =
	         BgVal.info =
	         Rule.info
               sharing type Name.name = 
			    NameSet.elt =
			    Link.name =
			    Ion.name =
			    Instantiation.name
               sharing type NameSet.Set = 
			    Interface.nameset =
			    BgVal.nameset =
			    Link.nameset =
			    Ion.nameset =
			    Wiring.nameset =
			    Permutation.nameset
	       sharing type Interface.interface =
			    BgVal.interface =
			    Instantiation.interface
               sharing type Link.link = LinkSet.elt
               sharing type LinkSet.Set = Wiring.linkset
	       sharing type Control.control = Ion.control
	       sharing type Ion.ion = BgVal.ion
               sharing type Wiring.wiring = BgVal.wiring
               sharing type Permutation.permutation = BgVal.permutation
               sharing type Permutation.Immutable = BgVal.Immutable
	       sharing type NameSet.Set = NameSetPP.collection
	       sharing type BgVal.bgval =
	        BgBDNF.bgval =
	        Rule.bgval
	       sharing type BgBDNF.bgbdnf = Rule.bgbdnf
	       sharing type BgBDNF.BR = Rule.BR
	       sharing type Instantiation.inst = Rule.inst
	       ) : SUGAR 
where type bgval = BgVal.bgval
  and type rule = Rule.rule
  and type ctrlkind = Control.kind =
struct
type control = Control.control
type name = string
type nameset = NameSet.Set
type bgval = BgVal.bgval
type arities = {boundarity : int, freearity : int}
type namedports = {boundports : string list, (* both lists should be sorted *)
                     freeports  : string list,
                     arities    : arities}
type portassign = {port : string, link : name}
type placeinfo = int * Name.name list
datatype mapinfo = |--> of placeinfo * placeinfo
type absinfo = nameset
type ctrlkind = Control.kind
type rule = Rule.rule
type redexinst = bgval * mapinfo list

open Debug
open ErrorHandler

val file_origin = Origin.mk_file_origin
                    "$BPL/src/kernel/ast/sugar.sml"
                    Origin.NOPOS
fun mk_explainer errtitle (explainer : exn -> explanation list) e =
    Exp (LVL_USER, Origin.unknown_origin, mk_string_pp errtitle, explainer e)

exception WrongArity of string
fun explain_WrongArity (WrongArity msg) =
    Exp (LVL_USER, Origin.unknown_origin, mk_string_pp msg,
         [Exp (LVL_LOW, file_origin, pp_nothing, [])])
  | explain_WrongArity _ = raise Match
val _ = add_explainer explain_WrongArity

val Ion = BgVal.Ion' Info.noinfo
val Mer = BgVal.Mer Info.noinfo
val Wir = BgVal.Wir Info.noinfo
fun Per x = BgVal.Per Info.noinfo x
val Con = BgVal.Con Info.noinfo
val Abs = BgVal.Abs' Info.noinfo
val Ten = BgVal.Ten Info.noinfo
val Par = BgVal.Par Info.noinfo
val Pri = BgVal.Pri Info.noinfo
val Com = BgVal.Com' Info.noinfo

fun active k = k Control.Active
fun active0 K
  = Ion (Ion.make {ctrl = Control.make (K, Control.Active, 0, 0),
                   free = [], bound = []}) 

fun passive k = k Control.Passive
fun passive0 K
  = Ion (Ion.make {ctrl = Control.make (K, Control.Passive, 0, 0),
                   free = [], bound = []}) 

fun atomic k = k Control.Atomic
fun atomic0 K
  = Com (Ion (Ion.make {ctrl = Control.make (K, Control.Atomic, 0, 0),
                        free = [], bound = []}), 
	 Mer 0)

exception DuplicateName of string * name list * name list list * string
fun explain_DuplicateName (DuplicateName (K, ys, Xs, errtxt)) =
    let
      fun pp_ion indent pps =
          let
            open PrettyPrint
            fun string_pp indent pps s = add_string pps s
            fun << () = begin_block pps CONSISTENT indent
            fun >> () = end_block pps
            fun brk () = add_break pps (0, 0)
          in
            <<();
            add_string pps K;
            case Xs of
              [] =>
              (case ys of
                 [] => ()
               | _  => mk_list_pp' "[" "]" "," string_pp indent pps ys)
            | _ =>
              (brk();
               mk_list_pp' "[" "]" "," string_pp indent pps ys;
               brk();
               mk_list_pp' "[" "]" ","
                           (mk_list_pp' "[" "]" "," string_pp) indent pps Xs);
            >>()
          end
    in
      [Exp (LVL_USER, Origin.unknown_origin, pp_ion, []),
       Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    end
  | explain_DuplicateName _ = raise Match
val _ = add_explainer
          (mk_explainer "duplicate name in ion" explain_DuplicateName)

exception DuplicatePort of string list * string list * string
fun explain_DuplicatePort (DuplicatePort (boundports, freeports, errtxt)) =
    let
      fun pp_ctrl_sig indent pps =
          let
            open PrettyPrint
            fun string_pp indent pps s = add_string pps s
            fun << () = begin_block pps CONSISTENT indent
            fun >> () = end_block pps
            fun brk () = add_break pps (0, 0)
          in
            <<();
            mk_list_pp' "[" "]" "," string_pp indent pps boundports;
            brk();
            add_string pps "--->";
            brk();
            mk_list_pp' "[" "]" "," string_pp indent pps freeports;
            >>()
          end
    in
      [Exp (LVL_USER, Origin.unknown_origin, pp_ctrl_sig, []),
       Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    end
  | explain_DuplicatePort _ = raise Match
val _ = add_explainer
          (mk_explainer "duplicate port name in control" explain_DuplicatePort)

fun listToString printfun ns =
    "[" 
    ^ #1 (foldr (fn (n, (s, notlast)) =>
		    (printfun n ^ (if notlast then ", " else "") ^ s,
		     true))
		("]", false)
		ns)

val namelistToString = listToString (fn s => s)
val namelistlistToString = listToString namelistToString

fun mkion ctrl free bound =
    let
      val ion
        = Ion (Ion.make
                   {ctrl  = ctrl,
                    free  = map Name.make free, 
                    bound = map (NameSet.fromList o map Name.make) bound})
          handle 
          NameSet.DuplicatesRemoved
          => raise DuplicateName 
                       (Control.name ctrl, free, bound,
                        "Duplicate names are not allowed in ions")
    in
      case Control.kind ctrl of
        Control.Atomic =>
        let
          val X = (hd o Interface.loc o BgVal.innerface) ion
          val barrenroot
            = if NameSet.isEmpty X then
                Mer 0
              else
                Ten [Wir (Wiring.introduce X), Mer 0]
        in
          Com (ion, Abs (X, barrenroot))
        end
      | _ => ion
    end
fun =: (K, {freearity, boundarity}) kind free bound =
    if length free <> freearity then
      raise WrongArity ("Control " ^ K ^ " takes "
			^ Int.toString freearity 
			^ " free names, but was given `"
			^ namelistToString free ^ "'\n")
    else if length bound <> boundarity then
      raise WrongArity ("Control " ^ K ^ " takes "
			^ Int.toString boundarity 
			^ " bound name sets, but was given `"
			^ namelistlistToString bound ^ "'\n")
    else
      mkion (Control.make (K, kind, boundarity, freearity)) free bound
fun ==: (K, {freeports, boundports, arities}) kind free bound =
  let
    fun unique_names [] = true
      | unique_names [_] = true
      | unique_names (n1::(rest as (n2::_))) 
        = String.< (n1, n2) andalso unique_names rest
    val boundports = ListSort.sort String.compare boundports
    val freeports  = ListSort.sort String.compare freeports
    fun compare ({port = port1, link = _}, {port = port2, link = _})
      = String.compare (port1, port2)
    val free  = ListSort.sort compare free
    val bound = ListSort.sort compare bound

    (* verify the given port assignments and extract the assigned names *)
    fun verify_ports _    wrap [] []    = []
      | verify_ports bOrF wrap ports [] =
        raise WrongArity ("Named " ^ bOrF ^ " ports `"
                          ^ namelistToString ports
                          ^ "' of control " ^ K ^ " were unspecified\n")
      | verify_ports bOrF wrap [] pas   =
        raise WrongArity (bOrF ^ " port assignments `"
                          ^ listToString
                              (fn {port, link} => port ^ " == " ^ link)
                              pas
                          ^ "' are superfluous\n")
      | verify_ports bOrF wrap (port'::ports) ({port, link}::pas) =
        case String.compare (port', port) of
          EQUAL   => (wrap link) :: (verify_ports bOrF wrap ports pas)
        | LESS    => raise WrongArity ("Control " ^ K
                                       ^ " has " ^ bOrF ^ " named port `" ^ port'
                                       ^ "' which is not assigned\n")
        | GREATER => raise WrongArity (bOrF ^ " port name `" ^ port
                                       ^ "' is not in the signature for control "
                                       ^ K ^ "\n")

      val _ = if unique_names boundports andalso unique_names freeports then
                ()
              else
                raise DuplicatePort (boundports, freeports,
                                     "Duplicate port names are \
                                     \not allowed in controls")
      val free'  = verify_ports "free"  (fn x => x)   freeports  free
      val bound' = verify_ports "bound" (fn x => [x]) boundports bound
  in
    mkion (Control.make' (K, kind, boundports, freeports)) free' bound'
  end
fun -: (K, freearity) kind free =
    =: (K, {freearity = freearity, boundarity = 0}) kind free []
fun --: (K, freeports) kind free =
    ==: (K, {freeports = freeports, boundports = [],
             arities = {freearity = length freeports, boundarity = 0}})
        kind free []

fun --> (boundarity, freearity) = {freearity  = freearity,
				                           boundarity = boundarity}
fun ---> (boundports, freeports) =
    {boundports = boundports, freeports = freeports,
     arities = {freearity  = length freeports,
                boundarity = length boundports}}
fun == (port, link) = {port = port, link = link}

val <-> = Mer 0
val op @ = Per o Permutation.make o map (fn j => (j, NameSet.empty))
val @@ = Per o Permutation.make o map (fn (j, Xs) => (j, NameSet.fromList Xs))
fun & (j, Xs) = (j, map Name.make Xs)
val merge = Mer
fun ` Xs _ = Con (NameSet.fromList (map Name.make Xs))
fun // (y, Xs) = 
  let
    val ls = 
      if y = "" then
        LinkSet.fromList
         (map 
           (fn x =>
            Link.make 
             {outer = NONE,
              inner = NameSet.singleton (Name.make x)})
            Xs)
      else
       (LinkSet.singleton 
			  (Link.make 
			    {outer = SOME (Name.make y),
			     inner = NameSet.fromList (map Name.make Xs)}))
	in
    Wir (Wiring.make ls)
  end
fun op / (y, x) = Wir (Wiring.make 
			 (LinkSet.singleton 
			    (Link.make 
			       {outer 
				= if y = "" then
				    NONE
				  else
				    SOME (Name.make y),
				inner = NameSet.singleton
					  (Name.make x)})))
fun op < Xs = NameSet.fromList (map Name.make Xs)
fun op > (X, P) = Abs (X, P)
val idp = Per o Permutation.id_n
val idw = Wir o Wiring.id_X o NameSet.fromList o map Name.make
val idw0 = Wir (Wiring.id_0)
val idx0 = Ten []
val op o = Com
fun op * (b1, b2) = Ten [b1, b2]
val ** = Ten
fun || (b1, b2) = Par [b1, b2]
val ||| = Par
fun `|` (b1, b2) = Pri [b1, b2]
val `|`` = Pri
fun -/ x = "" / x
fun -// X = // ("", X)
infix 2 |-->  infix 2 |->
fun i |-> j = (i, []) |--> (j, [])
infix 3 --  infix 3 --|>
fun redex -- mapinfos = (redex, mapinfos)
fun (redex, mapinfos) --|> react =
  let
    val I = BgVal.innerface redex
    val J = BgVal.innerface react
    val redex = BgBDNF.regularize (BgBDNF.make redex)
    (* FIXME check that the innerfaces are local and if not then give a
     *       reasonable error message *)
    val inst = Instantiation.make
     {I = I,
      J = J,
      maps = map (fn i |--> j => (i, j)) mapinfos}
  in
    Rule.make {name = "", redex = redex, react = react, inst = inst, info = Info.noinfo}
  end
infix 3 ----|>
fun redex ----|> react = redex --[]--|> react
infix 2 :::
fun rulename ::: rule =
  let
    val {redex, inst, react, ...} = Rule.unmk rule
  in
    Rule.make 
     {name = rulename, redex = redex, inst = inst, react = react, info = Info.noinfo}
  end

exception NonAtomicControl of bgval
fun explain_NonAtomicControl (NonAtomicControl m) =
    [Exp (LVL_USER, Origin.unknown_origin, pack_pp_with_data BgVal.pp m, [])]
  | explain_NonAtomicControl _ = raise Match
val _ = add_explainer
          (mk_explainer
             "control must be atomic when using the << >> operator"
             explain_NonAtomicControl)
exception NotMolecule of bgval
fun explain_NotMolecule (NotMolecule m) =
    [Exp (LVL_USER, Origin.unknown_origin, pack_pp_with_data BgVal.pp m, [])]
  | explain_NotMolecule _ = raise Match
val _ = add_explainer
          (mk_explainer
             "the << >> operator can only be used on atomic molecules"
             explain_NotMolecule)

val >> = ()
local open BgVal in
  fun << m >>
    = case match (PCom (PIon, PVar)) m of
        MCom (MIon ion, _) =>
        if Control.kind (#ctrl (Ion.unmk ion)) = Control.Atomic then
          Ion Info.noinfo ion
        else
          raise NonAtomicControl m
      | _ => raise NotMolecule m;
end

  
fun ppMapinfo indent pps ((i, xs) |--> (j, ys)) =
  let
    open PrettyPrint
    val show = add_string pps 
    fun << () = begin_block pps INCONSISTENT indent
	  fun >> () = end_block pps
	  fun <<< () = begin_block pps CONSISTENT indent
	  fun >>> () = end_block pps
	  fun brk () = add_break pps (1, 0)
	  fun brk0 () = add_break pps (0, 0)
  	fun pplist pp_y ys =
	    (<<(); show "[";
	     foldl (fn (y, notfirst) => 
		       (if notfirst then (show ","; brk()) else ();
			pp_y y;
			true)) false ys;
	     show "]"; >>())
  in
    <<();
    (case (xs, ys) of
      ([], []) =>
      (show (Int.toString i);
       brk(); show "|-> ";
       show (Int.toString j))
    | _ =>
      (show (Int.toString i ^ "&");
       pplist (Name.pp indent pps) xs;
       brk(); show "|--> ";
       show (Int.toString j ^ "&");
       pplist (Name.pp indent pps) ys));
    >>()
  end
val revision
  = hd (String.tokens (fn c => not (Char.isDigit c)) "$LastChangedRevision$")
end


functor Sugar (structure Info : INFO
	       structure Name : NAME
	       structure NameSet : MONO_SET
	       structure Interface : INTERFACE
	       structure Link : LINK
	       structure LinkSet : MONO_SET
	       structure Control : CONTROL
	       structure Ion : ION
	       structure Wiring : WIRING
	       structure Permutation : PERMUTATION
	       structure BgVal : BGVAL
	       structure BgBDNF : BGBDNF
	       structure Rule : RULE
	       structure Instantiation : INSTANTIATION
	       structure ErrorHandler : ERRORHANDLER
                 where type ppstream    = PrettyPrint.ppstream
                   and type break_style = PrettyPrint.break_style
                   and type origin      = Origin.origin
	       structure NameSetPP : COLLECTIONPRETTYPRINT
                 where type ppstream = PrettyPrint.ppstream
	       sharing type Info.info =
	         BgVal.info =
	         Rule.info
               sharing type Name.name = 
			    NameSet.elt =
			    Link.name =
			    Ion.name =
			    Instantiation.name
               sharing type NameSet.Set = 
			    Interface.nameset =
			    BgVal.nameset =
			    Link.nameset =
			    Ion.nameset =
			    Wiring.nameset =
			    Permutation.nameset
	       sharing type Interface.interface =
			    BgVal.interface =
			    Instantiation.interface
               sharing type Link.link = LinkSet.elt
               sharing type LinkSet.Set = Wiring.linkset
	       sharing type Control.control = Ion.control
	       sharing type Ion.ion = BgVal.ion
               sharing type Wiring.wiring = BgVal.wiring
               sharing type Permutation.permutation = BgVal.permutation
               sharing type Permutation.Immutable = BgVal.Immutable
	       sharing type NameSet.Set = NameSetPP.collection
	       sharing type BgVal.bgval =
	        BgBDNF.bgval =
	        Rule.bgval
	       sharing type BgBDNF.bgbdnf = Rule.bgbdnf
	       sharing type BgBDNF.BR = Rule.BR
	       sharing type Instantiation.inst = Rule.inst
	       ) :> SUGAR 
where type bgval = BgVal.bgval
  and type rule = Rule.rule
  and type ctrlkind = Control.kind =
struct
  structure Sugar = Sugar'(structure Info = Info
			   structure Name = Name
			   structure NameSet = NameSet
			   structure Interface = Interface
			   structure Link = Link
			   structure LinkSet = LinkSet
			   structure Control = Control
			   structure Ion = Ion
			   structure Wiring = Wiring
			   structure Permutation = Permutation
			   structure BgVal = BgVal
	       structure BgBDNF = BgBDNF
	       structure Rule = Rule
	       structure Instantiation = Instantiation
			   structure ErrorHandler = ErrorHandler
			   structure NameSetPP = NameSetPP)
  open Sugar
end
