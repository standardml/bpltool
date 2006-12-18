(* Prettyprinting structure for bg datatype of BgTerm module. 
   Mainly for debugging use.

Precedences (with tightest binding at bottom):

  o      composition
  x      tensor product
  ( )    abstraction

Notes: 
- Could also export PP-bg in sig - but as bg's aren't checked for
  malformnedness, there could be a larger amount of unintelligeble 
  exceptions.
- Prettyprint for perms aren't very 'pretty'
- Just noticed - might as use layoutSet fun in MonoSet-sig...
*)

functor PPBg ( structure BgTerm : BGTERM )
	: sig
	  val maxDepth : int ref
	  val PP_iface : BgTerm.I.iface -> PrettyPrint.StringTree
	  val PP_bgval : 'a BgTerm.bgval -> PrettyPrint.StringTree
	  val PP_bg : BgTerm.bg -> PrettyPrint.StringTree
	  exception PPError of string
	end =
struct

open BgTerm
open PrettyPrint
     
exception PPError of string

val maxDepth = ref 15
	       
val name2str = N.pp

fun PP_nset' depth nset = 
    if (depth > !maxDepth) then LEAF "#" else
    let val elts' = N.Set.list nset in
      NODE {start = "{",
	    children = map (LEAF o name2str) elts',
	    childsep = RIGHT ", ",
	    finish = "}",
	    indent = 2}
    end
      
fun PP_permEntry' depth (nset, i) =
    NODE {start = "(" ,
	  children = [LEAF (Int.toString i), PP_nset' (depth + 1) nset],
	  childsep = RIGHT ", ",
	  finish = ")",
	  indent = 2}
    
fun PP_locals' depth nsets =
    if (depth > !maxDepth) then LEAF "#" else
    NODE
      {start = "[",
       children = map (PP_nset' (depth (* +1 *))) nsets,
       childsep =  RIGHT ", ",
       finish = "]",
       indent = 0}
      
fun PP_ports' depth pp_elt elts =
    if (depth > !maxDepth) then LEAF "#" else
    NODE {start = "<",
	  children = map pp_elt elts,
	  childsep = RIGHT ", ",
	  finish = ">",
	  indent = 2}
    
fun PP_iface' depth i =
    if (depth > !maxDepth) then LEAF "#"
    else
      NODE
	{start = "<", 
	 children = [LEAF (Int.toString(I.width i)),
		     PP_locals' (depth + 1) (I.loc i),
		     PP_nset' (depth + 1) (I.glob i)],
	 childsep =  RIGHT ", ",
	 finish = ">",
	 indent = 0}
	
fun PP_bg' depth lprec rprec bg = 
    let
      fun pp_bg' depth lprec rprec Barroot =
	  LEAF "()"
	| pp_bg' depth lprec rprec Merge =
	  LEAF "merge"
	| pp_bg' depth  lprec rprec (Conc( nset )) =
	  NODE {start = "'",
		  children = [PP_nset' (depth + 1) nset],
		  childsep = RIGHT ", ",
		  finish = "'",
		  indent = 0}
	| pp_bg' depth  lprec rprec (Wir(link)) =
	  let
	    val {outer, inner} = deconLink link
	    val nameopt = if isSome(outer) then (LEAF o name2str o valOf) outer
			  else LEAF ""
	  in
	    NODE {start = "",
		  children = [nameopt, PP_nset' depth inner],
		  childsep = LEFT "/",
		  finish = "",
		  indent = 0}
	  end
	| pp_bg' depth lprec rprec (Ion (ion)) =
	  let
	    val {ctrl, free, bound} = deconIon ion
	  in
	    NODE {start = (#name o deconCtrl) ctrl,
		  children = [PP_ports' (depth + 1) (LEAF o name2str) free,
			      PP_ports' (depth + 1) (PP_nset' (depth + 2)) bound],
		  childsep = LEFT "",
		  finish = "",
		  indent = 0}
	  end
	(* Note: Prettyprint for perms aren't very 'pretty' *)
	| pp_bg' depth lprec rprec (Perm (perm)) =
	  let
	    val rawPerms = deconPerm perm
	  in
	    NODE {start = "[",
		  children = map (PP_permEntry' depth) rawPerms,
		  childsep = RIGHT ", ",
		  finish = "]",
		  indent = 2}
	  end
	| pp_bg' depth lprec rprec (Abs (nset, bg)) =
	  let 
	    val prec = 4
	    val parentheses = lprec >= prec orelse rprec > prec
	  in
	    NODE {start = if parentheses then "(" else "",
		  children = [NODE{ start = "(",
				    children = [PP_nset' (depth + 1) nset],
				    childsep = NOSEP,
				    finish = ")",
				    indent = 0},
			      PP_bg' (depth + 1) prec 0 bg],
		  childsep = LEFT " ",
		  finish = if parentheses then ")" else "",
		  indent = 2 }
	  end
	| pp_bg' depth lprec rprec (OO (bg1, bg2)) =
	  let 
	    val prec = 2
	    val parentheses = lprec >= prec orelse rprec > prec
	  in
	    NODE {start = if parentheses then "(" else "",
		  children = [PP_bg' (depth + 1) 
				     (if parentheses then 0 else lprec) 
				     prec 
				     bg1,
			      PP_bg' (depth + 1) 
				     prec 
				     (if parentheses then 0 else rprec) 
				     bg2],
		  childsep = LEFT " o ",
		  finish = if parentheses then ")" else "",
		  indent = 2 }
	  end
	(* NOTE: XX is allowed to be unary or even nulary (see notes in version.txt) *)
	| pp_bg' depth lprec rprec (XX ([])) =
	  LEAF "id_eps" (* TODO: Better concrete syntax for id_epsilon *)
	| pp_bg' depth lprec rprec (XX (bg::nil)) = 
	  pp_bg' depth lprec rprec bg
	| pp_bg' depth lprec rprec (XX (bgs)) =  
	  let 
	    val prec = 3
	    val parentheses = lprec > prec orelse rprec > prec
	    (* NOTE: Might do lprec >= prec to reflect structure 
		     of bg-val more faithfully... *)
	    val precs = (if parentheses then 0 else lprec, prec) ::
			rev((prec,if parentheses then 0 else rprec) ::
			    (List.tabulate(length bgs - 2,
					   (fn _ => (prec,prec)))))
		(* Can't happen(!) (these cases handled above) -
		 but as code is under construction/in flux I raise an
                 exception nonetheless *) 
		handle size => raise PPError "Unexpected error while prettyprinting XX - case for two or more subterms reached with list of length 0 or 1." 
	  in
	    NODE {start = if parentheses then "(" else "",
		  children = map (fn (b,(lp,rp)) => PP_bg' (depth + 1) lp rp b)
				 (ListPair.zip (bgs,precs)),
				 childsep = LEFT " x ",
				 finish = if parentheses then ")" else "",
				 indent = 2 }
	  end

    in
      if (depth > !maxDepth) then LEAF "#" else
        pp_bg' depth lprec rprec bg
    end
      
(* Note: Isn't called rec. currently, but is prepared for it, by letting 
	 depth and prec. be specifiable. *)
fun PP_bgval' depth lprec rprec bv =
    if (depth > !maxDepth) then LEAF "#" else
    let
      val prec = 1 (* Precedence of : -> operators *)
      val parentheses = lprec >= prec
      val (b,i) = deconBgVal bv
    in
      NODE {start = if parentheses then "(" else "",
	    children = [ PP_bg' (depth + 1)
				(if parentheses then 0 else lprec)
				0
				b,
			NODE{ start = ": ",
			      children = [PP_iface' (depth + 1) (#inner i),
					  PP_iface' (depth + 1) (#outer i)],
			      childsep = LEFT " -> ",
			      finish = "",
			      indent = 0}],
	    childsep = LEFT " ",
	    finish = if parentheses then ")" else "",
	    indent = 0}
    end
      
fun PP_iface i = PP_iface' 0 i

fun PP_bg b = PP_bg' 0 0 0 b
		 
fun PP_bgval bv = PP_bgval' 0 0 0 bv
		  
end