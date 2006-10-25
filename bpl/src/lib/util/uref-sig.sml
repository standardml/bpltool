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

infix ::=

(** Interface to UnionFind package.
 * <p>
 * Union/Find data type with ref-like interface.  A Union/Find structure 
 * consists of a type constructor <code>'a uref</code> with operations for
 * making an element of <code>'a uref</code> (make), getting the contents of
 * an element (!!), checking for equality of two elements (equal), and
 * for joining two elements (union).  uref is analogous to ref as
 * expressed in the following table:
 * <table>
 * <th><td>type</td><td>'a ref</td><td>'a uref</td></th>
 * <tr><td>introduction</td><td>ref</td><td>uRef</td></tr>
 * <tr><td>elimination</td><td>!</td><td>!!</td></tr>
 * <tr><td>equality</td><td>=</td><td>equal</td></tr>
 * <tr><td>updating</td><td>:=</td><td>::=</td></tr>
 * <tr><td>unioning</td><td></td><td>link, union, unify</td></tr>
 * </table>
 * <p>
 * The main difference between <code>'a ref</code> and <code>'a
 * uref</code> is in the union operation.  Without union <code>'a
 * ref</code> and <code>'a uref</code> can be used interchangebly.  An
 * assignment to a reference changes only the contents of the
 * reference, but not the reference itself.  In particular, any two
 * pointers that were different (in the sense of the equality
 * predicate = returning false) before an assignment will still be so.
 * Their contents may or may not be equal after the assignment,
 * though.  In contrast, applying the union operations
 * (<code>link</code>, <code>union</code>, <code>unify</code>) to two
 * uref elements makes the two elements themselves equal (in the sense
 * of the predicate equal returning true).  As a consequence their
 * contents will also be identical: in the case of <code>link</code>
 * and <code>union</code> it will be the contents of one of the two
 * unioned elements, in the case of <code>unify</code> the contents is
 * determined by a binary function parameter.
 *
 * @author Fritz Henglein, DIKU, University of Copenhagen <henglein@diku.dk>
 * @contributor Henning Niss, IT University of Copenhagen (minor modifications) <hniss@itu.dk>
 * @version $LastChangedRevision: 129 $
 *)

signature URef =
  sig

	(** Type of uref-elements with contents of type 'a. *)  
    type 'a uref

      
	(** Creates a new element with the specified contents.
          * @params x
          * @param x content of element 
          *)
    val uRef: 'a -> 'a uref


	(** Compares two urefs for equality. 
          * equal (e, e') returns
	  * true if and only if e and e' are either made by the same
	  * call to uref or if they have been unioned (see below).
	  *)
    val equal: 'a uref * 'a uref -> bool

        (** Compares two urefs. 
         * compare cmp (e, e') returns EQUAL if and only if equal (e, e')
         * returns true; otherwise return cmp (x, x') where x is the
         * contents of e and x' is the contents of e'.
         * @params cmp e e'
         * @param cmp comparison function for uref contents
         *)
    val compare: ('a * 'a -> order) -> 'a uref * 'a uref -> order

	(** Returns the contents of a uref. 
	 * Note: if 'a is an equality type then !!(uref x) = x, and 
	 * equal(uref (!!x), x) = false.
	 *)
    val !! : 'a uref -> 'a

    val find : 'a uref -> 'a uref

	(** Updates the contents of a uref.
          * update(e, x) updates the contents of e to be x.
          *)
    val update : 'a uref * 'a -> unit
    val ::= : 'a uref * 'a -> unit

	(** Unifies two urefs.
         * unify f (e, e') makes e and e' equal; if v and v' are the 
	 * contents of e and e', respectively, before unioning them, 
	 * then the contents of the unioned element is f(v,v').
         * @params f e e'
         * @param f function combining the contents
	 *)
    val unify : ('a * 'a -> 'a) -> 'a uref * 'a uref -> unit

	(** Unions two urefs.
         * union (e, e') makes e and e' equal; the contents of the unioned
	 * element is the contents of one of e and e' before the union
         * operation.
	 * After union(e, e') elements e and e' will be congruent in the
	 * sense that they are interchangeable in any context.
	 *)
    val union : 'a uref * 'a uref -> unit

	(** Makes two urefs equal.
         * link (e, e') makes e and e' equal; the contents of the linked
	 * element is the contents of e' before the link operation
	 *)
    val link : 'a uref * 'a uref -> unit

  end; (* UREF *)
