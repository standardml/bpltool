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

(** Lazy list datatype.
 * @version $LastChangedRevision$
 *)

structure LazyList :> LAZYLIST =
struct
  exception EmptyList

  datatype 'a lazycell = Nil | Cons of 'a * (unit -> 'a lazycell)
  type 'a lazylist = unit -> 'a lazycell

  val lzNil = fn () => Nil
  
  fun lzCons t = fn () => Cons (t ())

  fun lzhd t = case t () of
                 Nil => raise EmptyList
               | Cons (head, _) => head
      
  fun lztl t = case t () of
                 Nil => raise EmptyList
               | Cons (_, tail) => tail

  fun lzmake t = t

  fun lzunmk t = t ()
        
  fun lzmap f t = fn () => case t () of
                             Nil => Nil
                           | Cons (elt, tail)
                              => Cons (f elt, lzmap f tail)
  
  fun lzfoldr f init =
    let
      fun fold t = case t () of
                     Nil => init
                   | Cons (elt, tail)
                      => f (elt, (fn () => fold tail))
    in
      fold
    end
  
  fun lzappend t1 t2 = fn () => case t1 () of
                                  Nil => t2 ()
                                | xs => xs
  
  fun lztake t 0 = lzNil
    | lztake t n = fn () => case t () of
                              Nil => raise Subscript
                            | Cons (elt, tail)
                               => Cons (elt, lztake tail (n - 1))
                               
  fun lzdrop t 0 = t
    | lzdrop t n = fn () => case t () of
                              Nil => raise Subscript
                            | Cons (_, tail) => lzdrop tail (n - 1) ()
  
  fun lzcombine ts = fn () =>
      let
        (* evaleds contains the evaluated values at this level,
         * notevaleds are remaining thunks to be eval'ed at this level.
         * nextrest is always called with at least one of these lists empty.
         * restcombs is a lazy list of combinations of lower levels.
         * Strategy: For the first element of restcombs, evaluate and add
         *            in turn each thunk at this level, adding it as the
         *            head of the first element.
         *            Save the values to which the thunks evaluated.
         *           For all following elements of restcomb, add in turn
         *            each of the saved values as the head of this element.
         *)
	      fun nextrest (evaleds : 'a list) (notevaleds : 'a lazylist,
	                                        restcombs : 'a list lazylist) = fn () =>
	        case restcombs () of
	          Nil => Nil
	        | Cons (restcomb, restcombtail) =>
	            let
				        fun addonnotevaled elts t
				          = case t () of
				              Nil => if null elts then Nil else nextrest elts (lzNil, restcombtail) ()
				            | Cons (elt, tail)
				               => Cons (elt :: restcomb, fn () => addonnotevaled (elt :: elts) tail)
	              fun addonevaled elts =
	                let
						        fun addonevaled' [] = addonnotevaled elts notevaleds
						          | addonevaled' (elt :: elts')
						            = Cons (elt :: restcomb, fn () => addonevaled' elts')
						      in
						        addonevaled' elts
						      end
				      in
				        addonevaled evaleds
				      end
      in
        lzunmk (foldl (nextrest []) (fn () => Cons ([], lzNil)) ts)
      end

  fun lztolist t = case t () of
                     Nil => []
                   | Cons (elt, tail) => elt :: lztolist tail
                   
  fun lzprint toStr t = 
    let
      fun printrest first t
        = case t () of
            Nil => print "]"
          | Cons (elt, tail)
             => (if first then () else print ", ";
                 print (toStr elt);
                 printrest false tail)
    in
      print "[";
      printrest true t
    end
end
