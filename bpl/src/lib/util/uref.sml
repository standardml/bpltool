(*
 *   Copyright 199x Fritz Henglein
 *   Copyright 2001 Henning Niss
 * 
 * Permission is hereby granted, to anyone and free of charge, to deal
 * in this software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the software, provided that
 * all copies or substantial portions of the software are accompanied
 * by the above copyright notice and this permission notice.
 * 
 * The software is provided "as is", without warranty of any kind, express
 * or implied, including but not limited to the warranties of
 * merchantability, fitness for a particular purpose and noninfringement.
 * In no event shall the above listed copyright holder(s) be liable for any
 * claim, damages or other liability, whether in an action of contract,
 * tort or otherwise, arising from, out of or in connection with the
 * software or the use or other dealings in the software.
 *)

(** UnionFind package.
 *
 * @author Fritz Henglein, DIKU, University of Copenhagen <henglein@diku.dk>
 * @contributor Henning Niss, IT University of Copenhagen (minor modifications) <hniss@itu.dk>
 *
 * @version $LastChangedRevision: 129 $
 *)

infix ::=
structure URef :> URef =
  struct

    (* I (hniss) introduced impossible and eliminated a
       lot of non-exhaustive matches. *)
    exception Impossible
    val impossible = fn () => raise Impossible

    datatype 'a urefC
      = ECR of 'a * int
      | PTR of 'a uref
    withtype 'a uref = 'a urefC ref

    fun find (p as ref (ECR _)) = p
      | find (p as ref (PTR p')) = let
	  val p'' = find p'
          in
	    p := PTR p''; p''
          end

    fun uRef x = ref (ECR(x, 0))

    fun !! p = case !(find p) of ECR(x,_) => x | _ => impossible()
    (* was: let val ECR(x, _) = !(find p) in x end *)
      
    fun equal (p, p') = (find p = find p')

    fun compare cmp (p, p') = 
	case (find p, find p') of
	    (p as ref (ECR(x,_)),p' as ref (ECR(x',_))) => 
	          if p = p' then EQUAL else cmp (x,x')
	  | _ => impossible()
    (* was:
	let val (p as ref (ECR(x,_)),p' as ref (ECR(x',_))) = (find p, find p')
	in  if p = p' then EQUAL else cmp (x,x')
	end
    *)

    fun update (p, x) = 
	case find p of
	    p' as ref(ECR(_, r)) => p' := ECR(x, r)
	  | _ => impossible()
    (* was: let val (p' as ref(ECR(_, r))) = find p
	  in
	    p' := ECR(x, r)
	  end
    *)
    val op ::= = update

    fun link (p, q) = let
	  val p' = find p
          val q' = find q
	  in
	    if (p' = q') then () else p' := PTR q
	  end

    fun unify f (p, q) = 
	case (find p, find q) of
	    (p' as ref(ECR(pc, pr)), q' as ref(ECR(qc, qr))) =>
            (* was:
                   let
	              val (p' as ref(ECR(pc, pr))) = find p
                      val (q' as ref(ECR(qc, qr))) = find q *)
            let val newC = f (pc, qc)
            in
		if p' = q'
		then p' := ECR(newC, pr)
		else if pr = qr
		then (
		      q' := ECR(newC, qr+1);
		      p' := PTR q')
		else if pr < qr
		then (
		      q' := ECR(newC, qr);
		      p' := PTR q')
		else ((* pr > qr *)
                      p' := ECR(newC, pr);
                      q':= PTR p')
            end
	  | _ => impossible()

    fun union (p, q) = let
	  val p' = find p
          val q' = find q
          in
	    if p' = q' 
              then ()
              else 
		  case (!p', !q') of
		      (ECR(pc, pr), ECR(qc, qr)) =>
		      (* was: let
                 		val ECR(pc, pr) = !p' and ECR(qc, qr) = !q'
		              in
                      *)
		      if pr = qr 
		      then (
			    q' := ECR(qc, qr+1);
			    p' := PTR q')
		      else if pr < qr
		      then p' := PTR q'
		      else q':= PTR p'
		    | _ => impossible()
          end

  end

