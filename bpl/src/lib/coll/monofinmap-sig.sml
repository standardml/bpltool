(** Finite maps with explicit domain type *)

signature MONO_FINMAP =
  sig
    type dom
    type 'b map

    (** Signal that an insert changed the data of an existing key. *)
    exception DATACHANGED

    val empty      : 'b map
    val singleton  : dom * 'b -> 'b map
    val isEmpty    : 'b map -> bool
    val lookup     : 'b map -> dom -> 'b option
    val inDomain   : dom -> 'b map -> bool
    (** Add a key-data pair k0 |-> d0.  Any existing key-data
     * pair k0 |-> d1 will silently be overwritten. 
     *)
    val add        : dom * 'b * 'b map -> 'b map
		(** Add a key-data pair k0 |-> d0.
		 * @params eq k0 d0 map
		 * @param eq  Equality operator to use on data pairs
		 *  @exception DATACHANGED  If a pair k0 |-> d1
		 *                          already exists with d0 <> d1.
		 *)
    val add'       : ('b * 'b -> bool) -> dom * 'b * 'b map -> 'b map
    val plus       : 'b map * 'b map -> 'b map
    val remove     : dom * 'b map -> 'b map option      
    val dom        : 'b map -> dom list
    val range      : 'b map -> 'b list
    val list       : 'b map -> (dom * 'b) list
    val fromList   : (dom * 'b) list -> 'b map
    val composemap : ('b -> 'c) -> 'b map -> 'c map
    val ComposeMap : (dom * 'b -> 'c) -> 'b map -> 'c map
    val fold       : (('a * 'b) -> 'b) -> 'b -> 'a map -> 'b
    val Fold       : (((dom * 'b) * 'c) -> 'c)-> 'c -> 'b map -> 'c
    val filter     : (dom * 'b -> bool) -> 'b map -> 'b map

    (** addList l m; adds a list of associations to a map. *)
    val addList : (dom * 'b) list -> 'b map -> 'b map

    (** mergeMap f m1 m2; merges two finite maps, with a composition 
       function to apply to the codomains of domains which clash. *)
    val mergeMap : (('b * 'b) -> 'b) -> 'b map -> 'b map -> 'b map

    exception Restrict of string
    (** @exception Restrict if an element
       of the list is not in the domain of the map. *)
    val restrict : (dom -> string) * 'b map * dom list -> 'b map

    (** enrich en (A, B) returns true if for all a and b 
       such that b \in B and a \in (A \restrict dom(B)) 
	     we have en(a,b). *)
    val enrich : ('b * 'b -> bool) -> ('b map * 'b map) -> bool
  end
