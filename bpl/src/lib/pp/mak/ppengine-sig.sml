(*
 *   Copyright 2001 Henning Makholm
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

signature PPengine = sig
  type 'a pptree
  (* An 'a pptree is a specification of pretty-printed output
   * with annotation commands of type 'a embedded
   *)

  val annotate : 'a -> 'a pptree -> 'a pptree
  (* annotate a t  wraps t in an a annotation
   *)
       
  val empty : 'a pptree

  val prefix : string -> 'a pptree -> 'a pptree
  (* prefix s t  is laid out as   sssttttttt
   *                                 ttttttt
   *)

  val postfix : string -> 'a pptree -> 'a pptree
  (* postfix s t  is laid out as   tttttttt
   *                               ttttsss
   *)

  val compose : 'a pptree * int * int * int * 'a pptree -> 'a pptree
  (* compose(t,n,h,k,u)  is laid out as  tttttt(n spaces)uuuuu
   *                                                     uuuuu
   *   or, if the height of t is at least h lines:
   *			                 ttttttttttttttt
   *                                     ttttttttttttttt
   *                                     (k spaces)uuuuu
   *                                               uuuuu
   *)

  val makelist : bool * string -> 'a pptree list -> 'a pptree
  (* makelist (true,s) [t,u,v]  is laid out as  ttttssuuuussvvvv
   *                                        or  ttttt
   *                                            ttttt
   *                                          ssuuuuu
   *                                            uuuuu
   *                                          ssvvvvv
   *                                            vvvvv
   * makelist (false,s) [t,u,v] is an "inconsistent" list, meaning that
   * in the multi-line format, several t's may share a single line.
   *)

  val forcemulti : 'a pptree -> 'a pptree
  (* forcemulti t is laid out as t, except that, for the purpose of
   * choosing list layouts, the entire package never counts as a
   * single-line item.
   *)

  val isPrinting : 'a pptree -> bool
  
  (* -------------------------------------------------------------- *)

  val format : 'b * ('a * 'b -> 'b)
               -> { vindentpenalty : int
                  , vindentwidth : int
	          , width : int
	   	  }
	       -> 'a pptree
               -> { vindent : int
		  , indent : int
		  , contents : ('b * string) list
		  } list

  val noformat : 'b * ('a * 'b -> 'b) -> 'a pptree -> ('b * string) list

end

infix ^+ +^ ++

signature PPbuild = sig
  type 'a pptree'
  (* These are convenience definitions.
   * No encapsulation is implied.
   * They can be freely mixed with PPengine primitives.
   *)
  
  val ppString : string -> 'a pptree'
      
  val ^+ : string * 'a pptree' -> 'a pptree'
  (* s ^+ t  is laid out as   sssttttt
   *                             ttttt
   *)
  
  val +^ : 'a pptree' * string -> 'a pptree'
  (* t +^ s  is laid out as   tttttttt
   *                          ttttsss
   *)

  val bracket : string -> 'a pptree' -> 'a pptree'
  (* bracket a#b  t is laid out as  aatttttt
   *                                  ttttbb
   *)
  
  val ++ : 'a pptree' * 'a pptree' -> 'a pptree'
  (* t ++ u  is laid out as   ttttt uuuuuu
   *                                uuuuuu
   *                     or   ttttttt
   *                          ttttttt
   *                            uuuuu
   *                            uuuuu
   *)
  
  val break : int * int -> 'a pptree' * 'a pptree' -> 'a pptree'
  (* break(n,k)(t,u)  is laid out as   ttttt(n spaces)uuuuuu
   *                                                  uuuuuu
   *                              or   ttttttttttttttt
   *                                   ttttttttttttttt
   *                                   (k spaces)uuuuu
   *                                             uuuuu
   *)
  
  val close : int * string -> 'a pptree' -> 'a pptree'
  (* close(n,s) t  is laid out as    ttttt(n spaces)sss
   * or if t has at least 2 lines:   ttttttttt
   *                                 ttttttttt
   *                                 ssss
   * Used to place "end"s and similar tokens
   *)

  val clist : string -> ('x -> 'a pptree') -> 'x list -> 'a pptree'
  (* clist a#b  [x,y,z]  is laid out as  xxxxaabbyyyyaabbzzzz
   *                                 or  xxxxx
   *                                     xxaa
   *                                   bbyyyyy
   *                                     yyyyaa
   *                                   bbzzzzz
   *                                     zzzzz
   *)

  val ilist : string -> ('x -> 'a pptree') -> 'x list -> 'a pptree'
  (* ilist is the inconsistent version of clist
   *)
  
end


