(* Rbset -- functional sets using Okasaki-style red-black trees *)
(* Ken Friis Larsen <kfl@it.edu>                                *)
(* Various extensions, and test: sestoft@dina.kvl.dk * 2001-10-21 *)

functor Rbset(type t
              val compare : t * t -> order
             ) : MONO_SET =
struct

  type item = t
  type elt = item

  datatype tree = LEAF
                | RED   of item * tree * tree
                | BLACK of item * tree * tree
                           
  type Set  = tree * int

  datatype intv = 
      All
    | From of item
    | To   of item
    | FromTo of item * item

  exception NotFound

  val empty = (LEAF, 0)

  fun size (_, n) = n

  fun singleton x = (BLACK(x, LEAF, LEAF), 1)

  fun isEmpty (LEAF, _) = true
    | isEmpty _         = false

  fun all (f: elt -> bool) ((t, _): Set) =
    let
      fun all' LEAF = true
        | all' (RED (y, l, r)) =
          all' l andalso f y andalso all' r
        | all' (BLACK (y, l, r)) =
          all' l andalso f y andalso all' r
    in
      all' t
    end

  exception Empty
  fun someElement (LEAF, _) = raise Empty
    | someElement (RED (x, _, _), _) = x
    | someElement (BLACK (x, _, _), _) = x

  fun member elm (tree, n) =
      let fun memShared x left right =
              case compare(elm,x) of
                  EQUAL   => true
                | LESS    => mem left
                | GREATER => mem right
          and mem LEAF                    = false
            | mem (RED(x, left, right))   = memShared x left right
            | mem (BLACK(x, left, right)) = memShared x left right
      in  mem tree end

  fun retrieve set x = if member x set then x else raise NotFound

  fun peek set x = if member x set then SOME x else NONE

  fun lbalance z (RED(y,RED(x,a,b),c)) d =
      RED(y,BLACK(x,a,b),BLACK(z,c,d))
    | lbalance z (RED(x,a,RED(y,b,c))) d =
      RED(y,BLACK(x,a,b),BLACK(z,c,d))
    | lbalance x left right = BLACK(x, left, right)
                              
  fun rbalance x a (RED(y,b,RED(z,c,d))) =
      RED(y,BLACK(x,a,b),BLACK(z,c,d))
    | rbalance x a (RED(z,RED(y,b,c),d)) =
      RED(y,BLACK(x,a,b),BLACK(z,c,d))
    | rbalance x left right = BLACK(x, left, right)
                        
  exception DuplicatesRemoved
    
  fun insert0 elm =
      let fun ins LEAF = RED(elm,LEAF,LEAF)
	    | ins (BLACK(x,left,right)) =
              (case compare(elm, x) of
                   LESS    => lbalance x (ins left) right
                 | GREATER => rbalance x left (ins right)
                 | EQUAL   => raise DuplicatesRemoved)
	    | ins (RED(x,left,right)) =
              (case compare(elm, x) of
                   LESS    => RED(x, (ins left), right)
                 | GREATER => RED(x, left, (ins right))
                 | EQUAL   => raise DuplicatesRemoved)
      in  ins end

  fun insert elm (set as (tree, n)) =
      ( case insert0 elm tree of
            RED(e, l, r) => BLACK(e, l, r)
          | tree         => tree          
      , n+1)

  fun insert' elm set =
      insert elm set
      handle DuplicatesRemoved => set
	
  fun add (elm, set) = insert elm set		
  fun add' (elm, set) = insert' elm set     
  fun addList xs set = List.foldl add set xs
  fun addList' xs set = List.foldl add' set xs

  fun push LEAF stack = stack
    | push tree stack = tree :: stack

  fun pushNode x right stack = BLACK(x, LEAF, LEAF) :: push right stack

  fun getMin []             some none = none
    | getMin (tree :: rest) some none = 
      let fun descend tree stack =
	  case tree of
	      LEAF                  => getMin stack some none
	    | RED  (x, LEAF, right) => some x (push right stack)
	    | BLACK(x, LEAF, right) => some x (push right stack)
	    | RED  (x, left, right) => descend left (pushNode x right stack) 
	    | BLACK(x, left, right) => descend left (pushNode x right stack) 
      in descend tree rest end

(*   fun getMin []             some none = none
    | getMin (tree :: rest) some none = 
      case tree of
          LEAF                  => getMin rest some none
        | RED  (x, LEAF, right) => some x (push right rest)
        | BLACK(x, LEAF, right) => some x (push right rest)
        | RED  (x, left, right) => getMin(pushNode left x right rest) some none
        | BLACK(x, left, right) => getMin(pushNode left x right rest) some none
 *)

  fun getMax []             some none = none
    | getMax (tree :: rest) some none = 
      let fun descend tree stack =
	  case tree of
	      LEAF                  => getMax stack some none
	    | RED  (x, left, LEAF)  => some x (push left stack)
	    | BLACK(x, left, LEAF)  => some x (push left stack)
	    | RED  (x, left, right) => descend right (pushNode x left stack) 
	    | BLACK(x, left, right) => descend right (pushNode x left stack) 
      in descend tree rest end

(*   fun getMax []             some none = none
    | getMax (tree :: rest) some none = 
      case tree of
          LEAF                  => getMax rest some none
        | RED  (x, left, LEAF)  => some x (push left rest)
        | BLACK(x, left, LEAF)  => some x (push left rest)
        | RED  (x, left, right) => getMax(pushNode right x left rest) some none
        | BLACK(x, left, right) => getMax(pushNode right x left rest) some none
 *)
  fun fold get f e (tree, n) =
      let fun loop stack acc =
              get stack (fn x => fn stack => loop stack (f x acc)) acc
      in  loop (push tree []) e end

  fun foldl f = fold getMin f

  fun foldr f = fold getMax f

  fun foldUntil get f e (tree, n) =
      let fun loop stack acc =
	      get stack (fn x => fn stack => case f x acc of
						 (true,acc) => acc
					       | (false,acc) => loop stack acc)
	                acc
      in loop (push tree []) e end
  fun foldlUntil f = foldUntil getMin f
  fun foldrUntil f = foldUntil getMax f

  fun list set = foldr (fn x => fn xs => x::xs) [] set

  fun appAll get f (tree, n) =
      let fun loop stack = get stack (fn x => (f x; loop)) ()
      in  loop [tree] end

  fun apply f = appAll getMin f

  fun revapp f = appAll getMax f

  fun find p (tree, n) =
      let fun loop stack = 
              getMin stack (fn x => fn stack => 
                                       if p x then SOME x else loop stack) NONE
      in  loop (push tree []) end

  fun map f s =
      foldl (fn k => fn res => insert (f k) res) empty s 

  (*  Ralf Hinze's convert a sorted list to RB tree *)
  local 
      datatype digits = 
               ZERO
             | ONE of item * tree * digits
             | TWO of item * tree * item * tree * digits
                      
      fun incr x a ZERO                  = ONE(x, a, ZERO)
        | incr x a (ONE(y, b, ds))       = TWO(x, a, y, b, ds)
        | incr z c (TWO(y, b, x, a, ds)) =
          ONE(z, c, incr y (BLACK(x, a, b)) ds)
          
      fun insertMax(a, digits) = incr a LEAF digits
                        
      fun build ZERO                  a = a
        | build (ONE(x, a, ds))       b = build ds (BLACK(x, a, b))
        | build (TWO(y, b, x, a, ds)) c = build ds (BLACK(x, a, RED(y, b, c)))
  
      fun buildAll digits = build digits LEAF

      fun toInt digits =
          let fun loop ZERO power acc            = acc
                | loop (ONE(_,_,rest)) power acc = 
                  loop rest (2*power) (power + acc)
                | loop (TWO(_,_,_,_,rest)) power acc =
                  loop rest (2*power) (2*power + acc)
          in  loop digits 1 0 end 

      fun get stack = getMin stack (fn x => fn stack => SOME(x,stack)) NONE
  
      fun insRest stack acc =
          getMin stack (fn x => fn stack => insRest stack (insertMax(x,acc))) 
                 acc
 
  in  
  fun fromSortedList ls = 
      let val digits = List.foldl insertMax ZERO ls 
      in  (buildAll digits, toInt digits) end
          
  fun fromList ls = addList ls empty
  fun fromList' ls = addList' ls empty

  (* FIXME: it *must* be possible to write union, equal, isSubset,
            intersection, and difference more elegantly. 
  *)
  val FACTOR = 0
  fun union' s1 (_, 0) = s1
    | union' (_, 0) s2 = s2
    | union' s1 (BLACK(x,LEAF,LEAF), _) = insert' x s1
    | union' (BLACK(x,LEAF,LEAF), _) s2 = insert' x s2
    | union' (s1 as (t1, n1)) (s2 as (t2, n2)) =
      let fun loop x y stack1 stack2 res =
              case compare(x, y) of
                  EQUAL => 
                  let val res = insertMax(x, res)
                  in  case (get stack1, get stack2) of
                          (SOME(x, s1), SOME(y, s2)) => loop x y s1 s2 res
                        | (NONE, NONE)               => res
                        | (SOME _, _)                => insRest stack1 res
                        | (_, SOME _)                => insRest stack2 res 
                  end
                | LESS => 
                  let val res = insertMax(x, res)
                  in  case get stack1 of
                          NONE => insRest stack2 (insertMax(y, res))
                        | SOME(x, stack1) => loop x y stack1 stack2 res
                  end
                | GREATER => 
                  let val res = insertMax(y, res)
                  in  case get stack2 of
                          NONE => insRest stack1 (insertMax(x, res))
                        | SOME(y, stack2) => loop x y stack1 stack2 res
                  end
      in  (* FIXME: here is lots of room for optimizations *)
	  if n1<FACTOR*n2 then
	      foldl insert' s2 s1
	  else if n2<FACTOR*n1 then
	      foldl insert' s1 s2
	  else
              case (get [t1], get [t2]) of
		  (SOME(x, stack1), SOME(y, stack2)) => 
		  let val digits = loop x y stack1 stack2 ZERO
		  in  (buildAll digits, toInt digits) end
		| (_, SOME _) => s2
		| _           => s1 end  

  fun union s1 (_, 0) = s1
    | union (_, 0) s2 = s2
    | union s1 (BLACK(x,LEAF,LEAF), _) = insert x s1
    | union (BLACK(x,LEAF,LEAF), _) s2 = insert x s2
    | union (s1 as (t1, n1)) (s2 as (t2, n2)) =
      let fun loop x y stack1 stack2 res =
              case compare(x, y) of
                  EQUAL => raise DuplicatesRemoved
                | LESS => 
                  let val res = insertMax(x, res)
                  in  case get stack1 of
                          NONE => insRest stack2 (insertMax(y, res))
                        | SOME(x, stack1) => loop x y stack1 stack2 res
                  end
                | GREATER => 
                  let val res = insertMax(y, res)
                  in  case get stack2 of
                          NONE => insRest stack1 (insertMax(x, res))
                        | SOME(y, stack2) => loop x y stack1 stack2 res
                  end
      in  (* FIXME: here is lots of room for optimizations *)
	  if n1<FACTOR*n2 then
	      foldl insert s2 s1
	  else if n2<FACTOR*n1 then
	      foldl insert s1 s2
	  else
              case (get [t1], get [t2]) of
		  (SOME(x, stack1), SOME(y, stack2)) => 
		  let val digits = loop x y stack1 stack2 ZERO
		  in  (buildAll digits, toInt digits) end
		| (_, SOME _) => s2
		| _           => s1 end  


  fun intersect (s1 as (t1, n1)) (s2 as (t2, n2)) =
      let fun loop x y stack1 stack2 res =
              case compare(x, y) of
                  EQUAL => 
                  let val res = insertMax(x, res)
                  in  case (get stack1, get stack2) of
                          (SOME(x, s1), SOME(y, s2)) => loop x y s1 s2 res
                        | _                          => res
                  end
                | LESS => 
                  (case get stack1 of
                       NONE            => res
                     | SOME(x, stack1) => loop x y stack1 stack2 res)
                | GREATER => 
                  (case get stack2 of
                       NONE            => res
                     | SOME(y, stack2) => loop x y stack1 stack2 res)
      in  (* FIXME: here is lots of room for optimizations *)
          case (get [t1], get [t2]) of
              (SOME(x, stack1), SOME(y, stack2)) => 
              let val digits = loop x y stack1 stack2 ZERO
              in  (buildAll digits, toInt digits) end
            | _           => empty end  


  fun disjoint (s1:Set) (s2:Set) : bool =
      isEmpty (intersect s1 s2)

  fun difference (s1 as (t1, n1)) (s2 as (t2, n2)) =
      let fun loop x y stack1 stack2 res =
              case compare(x, y) of
                  EQUAL => 
                  (case (get stack1, get stack2) of
                       (SOME(x, s1), SOME(y, s2)) => loop x y s1 s2 res
                     | (SOME _, _)                => insRest stack1 res
                     | _                          => res)
                | LESS => 
                  let val res = insertMax(x, res)
                  in  case get stack1 of
                          NONE            => res
                        | SOME(x, stack1) => loop x y stack1 stack2 res
                  end
                | GREATER => 
                  (case get stack2 of
                       NONE => insRest stack1 (insertMax(x, res))
                     | SOME(y, stack2) => loop x y stack1 stack2 res)
      in  (* FIXME: here is lots of room for optimizations *)
          case (get [t1], get [t2]) of
              (SOME(x, stack1), SOME(y, stack2)) => 
              let val digits = loop x y stack1 stack2 ZERO
              in  (buildAll digits, toInt digits) end
            | (_, SOME _) => empty
            | _           => s1 end  
	
  fun eq (t1, _) (t2, _) =
      let fun loop x y stack1 stack2 =
              case compare(x, y) of
                  EQUAL => 
                  (case (get stack1, get stack2) of
                       (SOME(x, s1), SOME(y, s2)) => loop x y s1 s2
                     | (NONE, NONE)               => true
                     | _                          => false)
                | _ => false
      in  (* FIXME: here is lots of room for optimizations *)
          case (get [t1], get [t2]) of
              (SOME(x, stack1), SOME(y, stack2)) => loop x y stack1 stack2 
            | (NONE, NONE)                       => true
            | _                                  => false end

  fun compare0 ((t1, _), (t2, _)) =
      let fun loop x y stack1 stack2 =
	  case compare(x, y) of
	      EQUAL =>
                  (case (get stack1, get stack2) of
                       (SOME(x, s1), SOME(y, s2)) => loop x y s1 s2
                     | (NONE, NONE)               => EQUAL
                     | (NONE, _)                  => LESS
                     | (_, NONE)                  => GREATER)
	    | order => order
      in
          case (get [t1], get [t2]) of
              (SOME(x, stack1), SOME(y, stack2)) => loop x y stack1 stack2 
            | (NONE, NONE)                       => EQUAL
	    | (NONE, _)                          => LESS
	    | (_, NONE)                          => GREATER
      end

  fun isSubset ((t1, _), (t2, _)) =
      let fun loop x y stack1 stack2 =
              case compare(x, y) of
                  EQUAL => 
                  (case (get stack1, get stack2) of
                       (SOME(x, s1), SOME(y, s2)) => loop x y s1 s2
                     | (NONE, _)                  => true
                     | _                          => false)
                | LESS => false
                | GREATER => 
                  (case get stack2 of
                       SOME(y, stack2) => loop x y stack1 stack2
                     | NONE            => false)  
      in  (* FIXME: here is lots of room for optimizations *)
          case (get [t1], get [t2]) of
              (SOME(x, stack1), SOME(y, stack2)) => loop x y stack1 stack2
            | (NONE, _)                          => true
            | _                                  => false end

  end

  (* Function f must be strictly monotonically increasing on the
     elements of s; we check this requirement: *)

  exception NonMonotonic

  fun mapMono f s =
      let val fxs = foldl (fn x => fn res => f x :: res) [] s
	  fun sorted []         = true
	    | sorted (y1 :: yr) = 
	      let fun h x0 []       = true
		    | h x0 (x1::xr) = compare(x0, x1) = LESS andalso h x1 xr
	      in h y1 yr end
      in 
	  if sorted fxs then
	      fromSortedList fxs
	  else
	      raise NonMonotonic
      end
     
  (* Peter Sestoft's convert a sorted list to RB tree *)
  (* Did I write this?  I'm impressed, but let's ignore it for now. 

  fun fromSortedList' (compare, ls) = 
      let val len = List.length ls
	  fun log2 n = 
	      let fun loop k p = if p >= n then k else loop (k+1) (2*p)
	      in loop 0 1 end
	  fun h 0 _ xs = (LEAF, xs)
	    | h n d xs =
	      let val m = n div 2
	          val (t1, y :: yr) = h m       (d-1) xs
	          val (t2, zs)      = h (n-m-1) (d-1) yr
	      in (if d=0 then RED(y, t1, t2) else BLACK(y, t1, t2), zs) end
      in  (compare, 
	   case #1 (h len (log2 (len + 1) - 1) ls) of
                RED(x, left, right) => BLACK(x, left, right)
              | tree                => tree
          , len) 
      end
  *)
      
  (* delete a la Stefan M. Kahrs *)
 
  fun sub1 (BLACK arg) = RED arg
    | sub1 _ = raise Fail "Rbset.sub1: impossible"

  fun balleft y (RED(x,a,b)) c             = RED(y, BLACK(x, a, b), c)
    | balleft x bl (BLACK(y, a, b))        = rbalance x bl (RED(y, a, b))
    | balleft x bl (RED(z,BLACK(y,a,b),c)) = 
      RED(y, BLACK(x, bl, a), rbalance z b (sub1 c))
    | balleft _ _ _ = raise Fail "Rbset.balleft: impossible"

  fun balright x a             (RED(y,b,c)) = RED(x, a, BLACK(y, b, c))
    | balright y (BLACK(x,a,b))          br = lbalance y (RED(x,a,b)) br
    | balright z (RED(x,a,BLACK(y,b,c))) br = 
      RED(y, lbalance x (sub1 a) b, BLACK(z, c, br))
    | balright _ _ _ = raise Fail "Rbset.balright: impossible"

  (* [append left right] constructs a new tree t.
  PRECONDITIONS: RB left /\ RB right 
              /\ !e in left => !x in right e < x
  POSTCONDITION: not (RB t)
  *)
  fun append LEAF right                    = right
    | append left LEAF                     = left
    | append (RED(x,a,b)) (RED(y,c,d))     = 
      (case append b c of
	   RED(z, b, c) => RED(z, RED(x, a, b), RED(y, c, d))
         | bc           => RED(x, a, RED(y, bc, d)))
    | append a (RED(x,b,c))                = RED(x, append a b, c)
    | append (RED(x,a,b)) c                = RED(x, a, append b c) 
    | append (BLACK(x,a,b)) (BLACK(y,c,d)) = 
      (case append b c of
	   RED(z, b, c) => RED(z, BLACK(x, a, b), BLACK(y, c, d))
         | bc           => balleft x a (BLACK(y, bc, d)))
   
  fun remove x (set as (tree, n)) =
      let fun delShared y a b =
              case compare(x,y) of
                  EQUAL   => append a b
                | LESS    => (case a of
                                  BLACK _ => balleft y (del a) b
                                | _       => RED(y, del a, b))
                | GREATER => (case b of
                                  BLACK _ => balright y a (del b)
                                | _       => RED(y, a, del b))  
          and del LEAF             = raise NotFound
            | del (RED(y, a, b))   = delShared y a b
            | del (BLACK(y, a, b)) = delShared y a b
      in  ( case del tree of
                RED arg => BLACK arg
              | tree    => tree          
          , n-1) end

  fun remove' x set =
      (remove x set) handle NotFound => set

  fun min (t, _) = 
      let fun h LEAF = NONE
	    | h (RED  (k, LEAF, t2)) = SOME k
	    | h (RED  (k, t1,   t2)) = h t1
	    | h (BLACK(k, LEAF, t2)) = SOME k
	    | h (BLACK(k, t1,   t2)) = h t1
      in h t end

  fun max (t, _) = 
      let fun h LEAF = NONE
	    | h (RED  (k, t1, LEAF)) = SOME k
	    | h (RED  (k, t1, t2  )) = h t2
	    | h (BLACK(k, t1, LEAF)) = SOME k
	    | h (BLACK(k, t1, t2  )) = h t2
      in h t end

  fun hash (h : item -> word) (s : Set) = 
      foldl (fn k => fn res => h k + res) 0w0 s

  (* Extract sublist containing the elements that are in the given interval *)

  fun sublist (t, _) intv =
      let fun collectall LEAF res = res
	    | collectall (RED(k, t1, t2)) res = 
	      collectall t1 (k :: collectall t2 res)
	    | collectall (BLACK(k, t1, t2)) res = 
	      collectall t1 (k :: collectall t2 res)
	  (* Collect from `from' till end *) 
	  fun collectfrom LEAF res = res
	    | collectfrom (tree as RED  (k, t1, t2)) res =
	      collnode tree k t1 t2 res
	    | collectfrom (tree as BLACK(k, t1, t2)) res =
	      collnode tree k t1 t2 res
	  and collnode tree k t1 t2 res = 
	      case intv of 
		  From from => 
		      if compare(from, k) = GREATER then (* ignore left *)
			  collectfrom t2 res
		      else (* from <= k *)
			  collectfrom t1 (k :: collectall t2 res)
		| FromTo (from, _) => 
		      if compare(from, k) = GREATER then (* ignore left *)
			  collectfrom t2 res
		      else (* from <= k *)
			  collectfrom t1 (k :: collectfrom t2 res)
		| _ => collectall tree res
	  (* Collect from beginning to `to', exclusive *)
	  fun collectto LEAF res = res
	    | collectto (tree as RED  (k, t1, t2)) res = 
	      collnode tree k t1 t2 res
	    | collectto (tree as BLACK(k, t1, t2)) res =
	      collnode tree k t1 t2 res
	  and collnode tree k t1 t2 res =
	      case intv of 
		  To to => 
		      if compare(k, to) = LESS then
			  collectall t1 (k :: collectto t2 res)
		      else (* ignore right, k >= to *)
			  collectto t1 res
		| FromTo (_, to) => 
		      if compare(k, to) = LESS then
			  collectall t1 (k :: collectto t2 res)
		      else (* ignore right, k >= to *)
			  collectto t1 res
		| _ => collectall tree res
	  (* Collect from `from' to `to' *)
	  fun collectfromto LEAF res = res
	    | collectfromto (tree as RED  (k, t1, t2)) res =
	      collnode tree k t1 t2 res 
	    | collectfromto (tree as BLACK(k, t1, t2)) res =
	      collnode tree k t1 t2 res 
	  and collnode tree k t1 t2 res = 
	      case intv of 
		  From from => collectfrom tree res
		| To to     => collectto tree res
		| FromTo (from, to) => 
		      if compare(from, k) = GREATER then (* ignore left *)
			  collectfromto t2 res
		      else if compare(k, to) = LESS then  (* from <= k < to *)
			  collectfrom t1 (k :: collectto t2 res)
		      else (* ignore right *)
			  collectfromto t1 res
		| All => collectall tree res
      in collectfromto t [] end

    (* Note: builds an intermediate list of elements *)
    fun subset (s as (t, _)) intv = 
	fromSortedList(sublist s intv)

    (* For debugging only *)

    fun depth LEAF = 0
      | depth (RED  (_, t1, t2)) = 1 + Int.max(depth t1, depth t2)
      | depth (BLACK(_, t1, t2)) = 1 + Int.max(depth t1, depth t2)

    val depth = fn (_, t, _) => depth t

    (* to satisfy the interface *)
    val compare = compare0
    fun partition f s = raise Fail("Not implemented: partition")
    fun subst (a,b) s = raise Fail("Not implemented: subst")
    val fold = foldl
    val foldUntil = foldlUntil
    type StringTree = EdLibPrettyPrint.StringTree
    fun layoutSet {start, sep, finish} layoutItem s =
      EdLibPrettyPrint.NODE {start=start,
			     finish=finish,
			     indent=3,
			     childsep=EdLibPrettyPrint.RIGHT sep,
			     children=List.map layoutItem (list s)}


    fun tabulate n f =
      let
        fun tab' n' s =
          if n = n' then
            s
          else
            tab' (n' + 1) (insert' (f n') s)
      in
        if n < 0 then
          empty
        else
          tab' 0 empty
      end
end
