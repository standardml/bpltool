(*
 *   Copyright 2001 Henning Makholm
 *   Copyright 2002 Henning Niss
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

(*signature PPeninge = PPengine*)
structure PPengine :> PPengine = struct
  infix --

  fun dprint _ = ()
(*  val dprint = print  *)
  
  datatype 'a linear = LEAF of string
		     | SPACES of int
		     | NIL
		     | ENTERANNO
                     | EXITANNO of 'a
		     | -- of 'a linear * 'a linear
  datatype 'a pptree = LINEAR of 'a linear
		     | FORCEMULTI of 'a pptree
		     | COMPOSE of 'a pptree * int * int * int * 'a pptree
                     | LIST of 'a linear * bool * string * 'a pptree list

  fun Prefix L m (LINEAR Ls) = LINEAR(L--Ls)
    | Prefix L m (FORCEMULTI t) = FORCEMULTI(Prefix L m t)
    | Prefix L m (COMPOSE(t,n,h,k,u)) = COMPOSE(Prefix L m t,n,h,k+m,u)
    | Prefix L m (LIST(Ls,cons,s,ts)) = LIST(L--Ls,cons,s,ts)
  fun Postfix L (LINEAR Ls) = LINEAR(Ls--L)
    | Postfix L (FORCEMULTI t) = FORCEMULTI(Postfix L t)
    | Postfix L (COMPOSE(t,n,h,k,u)) = COMPOSE(t,n,h,k,Postfix L u)
    | Postfix L (LIST(Ls,cons,s,ts)) = let fun f [] = raise Match
     					     | f [t] = [Postfix L t]
					     | f (t::ts) = t :: f ts
				       in LIST(Ls,cons,s,f ts)
				       end
				       
  fun annotate a = Prefix ENTERANNO 0 o Postfix (EXITANNO a)
  val empty = LINEAR NIL
  fun prefix s = Prefix (LEAF s) (size s)
  fun postfix s = Postfix (LEAF s)
  val compose = COMPOSE
  fun makelist (cons,s) [] = empty
    | makelist (cons,s) [t] = t
    | makelist (cons,s) ts = LIST(NIL,cons,s,ts)
  val forcemulti = FORCEMULTI

  val printingS = List.exists (fn c => c <> #" ") o explode
  fun printingL (LEAF s) = printingS s
    | printingL (L1--L2) = printingL L1 orelse printingL L2
    | printingL _ = false
  fun isPrinting (LINEAR L) = printingL L
    | isPrinting (FORCEMULTI t) = isPrinting t
    | isPrinting (COMPOSE(t,_,_,_,u)) = isPrinting t orelse isPrinting u
    | isPrinting (LIST(L,_,s,ts)) = printingL L orelse
 				    List.exists isPrinting ts orelse
 				    (length ts >= 2 andalso printingS s)
		     
  fun max(a,b) = if a > b then a else b
  fun min(a,b) = if a > b then b else a
  fun spaces n = CharVector.tabulate(n, fn _ => #" ")
		     
  (* ------------------------------------------------------------------------
   * The formatting function tries to optimise thw length of the formatted
   * program globally. In doing so, it makes use of an auxiliary data
   * structure (describing one way to lay out a subtree) and a memoizer:
   *)

  fun lflatten L0 = let fun f (LEAF s    ) acc =  s  :: acc
			  | f (SPACES n  ) acc = " " :: acc
			  | f (ENTERANNO ) acc =        acc
			  | f (EXITANNO _) acc =        acc
			  | f NIL          acc =        acc
			  | f (L1 -- L2)   acc = f L1 (f L2 acc)
		    in concat ("<" :: f L0 [">"])
		    end
		    
  datatype 'a how = Line of 'a linear
		  | Break of 'a how * 'a subhow
       and 'a subhow = Indent of int * 'a subhow
		     | Text of string * 'a how
		     | Vindent of 'a how
  type 'a options = { minwidth : int
		    , linwidth : int
		    , linear : 'a linear
		    , get : int -> int * int * 'a how
		    }
  fun memoize verywide ({minwidth,linwidth,linear,get=oldget})
      = let datatype 'a arrelem = UNKNOWN
				| FOUND of (int * int * 'a how)
	    val linwidth = if linwidth > verywide then verywide else linwidth
	    val linanswer = (linwidth,1,Line linear)
	    val cache = Array.array(linwidth,UNKNOWN)
	    fun put(a,b) v = let val low = min(a,b)
			         fun at i = Array.update(cache,i,v)
				 fun f i = if i = low then at i
					   	      else ( at i; f (i-1) )
			     in f (min(max(a,b),linwidth-1))
			     end
	    fun compute width
 		= let val got as (rwidth,rheight,_) = oldget width
		      val (f,triple) = case if rwidth < linwidth
					    then Array.sub(cache,rwidth)
					    else UNKNOWN
				       of f as FOUND triple => (f,triple)
				       	| UNKNOWN => (FOUND got,got)
		  in put(rwidth,width) f
	      	   ; triple
		  end
	    fun get width = case Array.sub(cache,width)
			    of FOUND triple => triple
			     | UNKNOWN => compute width
	in { minwidth = minwidth
	   , linwidth = linwidth
	   , linear = linear
	   , get = fn width => if width >= linwidth
 			       then linanswer
			       else get (max(width,minwidth))
	   }
	end

  fun lwidth (LEAF s    ) = size s
    | lwidth (SPACES n  ) = n
    | lwidth (NIL       ) = 0
    | lwidth (L1--L2    ) = lwidth L1 + lwidth L2
    | lwidth (ENTERANNO ) = 0
    | lwidth (EXITANNO _) = 0

  fun hPrefix w1 L1 = let fun f (Line L2) = Line(L1--L2)
    			    | f (Break(how,sub)) = Break(f how,Indent(w1,sub))
		      in f end
(*
  fun danno ann (w,h,how) = (w,h,
			     Break(Line(LEAF(concat["·(",
						    Int.toString w,"x"
						   ,Int.toString h
						   ,") ",ann])),
				   Text("",how)))
*)
  fun format (topanno,acons) {vindentpenalty,vindentwidth,width} toptree =
      let
	(*--------------------------------------------------------------
	 * In the first phase of format, we convert each subtree to an
	 * options record
	 *)
	val infinity = width+1

        fun pass1 (LINEAR L0)
            = let val width = lwidth L0
		  val line = Line L0
	      in { minwidth = width
		 , linwidth = width
	       	 , linear = L0
		 , get = fn w => if w >= width
 				 then (width,1,line)
				 else (width-1,2*vindentpenalty,line)
		 }
	      end
	  | pass1 (FORCEMULTI t)
	    = let val {minwidth,linwidth,linear,get} = pass1 t
	      in { minwidth = minwidth, linwidth = infinity,
 		   linear = linear, get = get }
	      end
	  | pass1 (COMPOSE(t,n,h,k,u))
	    = let val tR = pass1 t
	  	  val uR = pass1 u
		  val wt1 = #linwidth tR + n
		  val Lt1 = #linear tR -- SPACES n
		  fun mk2t w' = let val s2t as (t2w,t2h,_) = #get tR w'
			       	in if t2h < h andalso t2w <= w'
				   then mk2t (w'-1) else s2t
			       	end
		  fun get w =
 		      let fun best (s1 as (w1,h1,_)) (s2 as (w2,h2,_))
			      = if w1 > w orelse w2 > w orelse h1 = h2
			      	then if w1 < w2 then s1 else s2
			      	else if h1 < h2 then s1 else s2
			  (* compute s2 first - if we get an u-layout
			   * that's narrow enough to be used in s1 it
			   * will not need to be recomputed.
			   *)
			  val s2 = let val (tw,th,thow) = mk2t w
				       val (uw,uh,uhow) = #get uR (w-k)
				   in (max(tw,uw+k),th+uh,
				       Break(thow,Indent(k,Text("",uhow))))
				   end
			  val s1 = if w > wt1
			       	   then let val (tw,th,thow) = #get tR w
					    val (uw,uh,uhow) = #get uR (w-wt1)
					in (wt1 + uw,uh,hPrefix wt1 Lt1 uhow)
				    	end
			       	   else (infinity + #1 s2,0,Line NIL)
			  val s12 = best s1 s2
		      in if (#1 s12 > w) orelse
			    (#2 s12 > vindentpenalty andalso
			     vindentwidth - w > 20)
			 then best s12
			      let val (tw,th,thow) = #get tR w
				  val (uw,uh,uhow) = #get uR vindentwidth
			      in (tw,th+vindentpenalty+uh,
				  Break(thow,Indent(k,Vindent uhow)))
			      end
			 else s12
		      end
	      in memoize infinity { minwidth = #minwidth tR
				  , linwidth = wt1 + #linwidth uR
		 	  	  , linear = Lt1 -- #linear uR
			  	  , get = get
			  	  }
	      end
	  | pass1 (LIST(L,_,s,[])) = pass1 (LINEAR L)
	  | pass1 (LIST(L,cons,s,t0::ts))
	    = let val R0 = pass1 t0
		  val Rs = map pass1 ts
		  val wL = lwidth L
		  val sn = size s
		  val sL = LEAF s
		  val realmin = wL + (foldl max
 				      (#minwidth R0) (map (#minwidth) Rs))
		  fun get w0 =
		      let val w = w0 - wL
			  fun shadd w2 L2 (revl as (w1,1,Line L1)::rest)
			      = if w1 + sn + w2 <= w
				then (w1+sn+w2, 1, Line(L1--sL--L2)) :: rest
				else             (w2,1,Line L2) :: revl
			    | shadd w2 L2 revl = (w2,1,Line L2) :: revl
		  	  fun add (R : 'a options,revl)
		      	      = if not cons andalso #linwidth R <= w
				then shadd (#linwidth R) (#linear R) revl
				else (#get R w) :: revl
			  val flist = rev (foldl add [] (R0::Rs))
		      in ( wL + foldl max 0 (map #1 flist)
			 ,      foldl op+ 0 (map #2 flist)
			 , hPrefix wL L (foldl (fn ((_,_,how1),how0)
				     	  	   => Break(how0,Text(s,how1)))
			        	 (#3 (hd flist)) (tl flist))
			 )
		      end
	      in memoize infinity
 		 { minwidth = wL
		 , linwidth = wL + (foldl (fn (R,w) => w + sn + #linwidth R)
			      	    (#linwidth R0) Rs)
	       	 , linear = L -- (foldl (fn (R,L) => L -- sL -- #linear R)
 			    	  (#linear R0) Rs)
		 , get = fn w0 => if w0 > realmin
				  then get w0
				  else let val (w,h,how) = get vindentwidth
				       in (w0,h+1+vindentpenalty,
					   Break(Line NIL,Vindent(how)))
				       end
		 }
	      end
	val topopts = pass1 toptree
	(*--------------------------------------------------------------
	 * In the second phase we use this structure to find a recipe
	 * for a good layout
	 *)
	val tophow = if #linwidth topopts <= width
		     then Line(#linear topopts)
		     else #3 (#get topopts width)
	(*--------------------------------------------------------------
	 * In the third phase, convert the recipe to a list of lines
	 *)
	fun closeoff (bs,[],rest) = rest
	  | closeoff (bs,ss,rest) = (hd bs,concat ss) :: rest
	fun forL (L,state as (bs,ss,tail))
 	    = case L of LEAF s     => (bs,   s     :: ss,tail)
	  	      | SPACES n   => (bs,spaces n :: ss,tail)
	  	      | NIL        => state
	  	      | ENTERANNO  => (        tl bs     ,[],closeoff state)
		      | EXITANNO a => (acons(a,hd bs)::bs,[],closeoff state)
		      | L1 -- L2   => forL(L1,forL(L2,state))
	fun forhow (vind,ind,pre,h,acc)
	    = case h of Line L      => let val (bs1,tail) = acc
					   val state as (bs0,_,_) =
 				   	       forL (LEAF pre--L, (bs1,[],[]))
	      		   	       in (bs0,{ vindent = vind
		      		   	       , indent = ind - size pre
		      		   	       , contents = closeoff state
		      		   	       } :: tail)
	      		   	       end
	       	      | Break(h,sh) => forhow(vind,ind,pre,h,
					      forsubhow(vind,ind,sh,acc))
	and forsubhow (vind,ind,sh,acc)
	    = case sh of Indent(n,sh') => forsubhow(vind,ind+n,sh',acc)
	    	       | Text(pre,h)   => forhow(vind,ind,pre,h,acc)
		       | Vindent h     => forhow(vind+ind,0,"",h,acc)

      in
	#2 (forhow (0,0,"",tophow,([topanno],[])))
      end

  fun noformat (topanno,acons) t
      = let fun I state = state
	    fun forS "" state = state
	      | forS s (bs,acc) = (bs,(hd bs,    s   )::acc)
	    fun forN 0 state = state
	      | forN n (bs,acc) = (bs,(hd bs,spaces n)::acc)
	    fun forL (LEAF s    ) = forS s
	      | forL (SPACES n  ) = forN n
	      | forL (NIL       ) = I
	      | forL (ENTERANNO ) = (fn (bs,acc) => (        tl bs     ,acc))
	      | forL (EXITANNO a) = (fn (bs,acc) => (acons(a,hd bs)::bs,acc))
	      | forL ( L1 -- L2  ) = forL L1 o forL L2
	    fun fortree (LINEAR L) = forL L
	      | fortree (FORCEMULTI t      ) = fortree t
	      | fortree (COMPOSE(t,n,_,_,u)) = fortree t o forN n o fortree u
	      | fortree (LIST(L,_,_,[])    ) = forL L
	      | fortree (LIST(L,_,s,t::ts) )
 	      	= forL L o
 		  fortree t o
 		  foldr (fn (t,R) => forS s o fortree t o R) I ts
      	in
	  #2 (fortree t ([topanno],[]))
      	end
end

infixr 5 ^+
infix 4 +^
infixr 4 ++

(* Added a prime to PPbuild, HN *)
signature PPbuild' = PPbuild where type 'a pptree' = 'a PPengine.pptree
structure PPbuild :> PPbuild' = struct
  open PPengine
  type 'a pptree' = 'a pptree

  fun ssplit s = let fun getab acc (#"#"::cs) = (implode (rev acc),implode cs)
	  	       | getab acc (c::cs) = getab (c::acc) cs
	  	       | getab acc [] = raise Fail ("ppbuild.ssplit:" ^ s)
  		 in getab [] (explode s)
		 end
		 
  fun ppString s = postfix s empty
  fun s ^+ t = prefix s t
  fun t +^ s = postfix s t
  fun bracket ab = let val (a,b) = ssplit ab in prefix a o postfix b end
  fun t ++ u = compose(t,1,0,2,u)
  fun break(n,k) (t,u) = compose(t,n,0,k,u)
  fun close(n,s) t = compose(t,n,2,0,ppString s)

  fun xlist cons ab f
      = let val (a,b) = ssplit ab
  	    fun convert [] = []
    	      | convert [x] = [f x]
    	      | convert (x::xs) = (f x +^ a) :: convert xs
	in makelist(cons,b) o convert
	end
  fun clist ab f = xlist true ab f
  fun ilist ab f = xlist false ab f
end
