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

(** Abstract data type for modelling names.
 * 
 * @version $LastChangedRevision$
 *)
functor Name'(structure ErrorHandler : ERRORHANDLER
                where type ppstream    = PrettyPrint.ppstream
                  and type break_style = PrettyPrint.break_style
                  and type origin      = Origin.origin)
 : NAME =
struct

  open Debug
  open ErrorHandler

  val file_origin = Origin.mk_file_origin
                      "$BPL/src/kernel/bg/name.sml"
                      Origin.NOPOS
  fun mk_explainer errtitle (explainer : exn -> explanation list) e =
      Exp (LVL_USER, Origin.unknown_origin, mk_string_pp errtitle,
           explainer e)

  (* Names are identified by unique words. *)
  type name = Word.word * string

  fun == ((id1, _) : name, (id2, _) : name) =
      id1 = id2

  fun lt ((id1, _) : name, (id2, _) : name) =
      id1 < id2

  fun compare ((id1, _) : name, (id2, _) : name) =
      Word.compare(id1,id2)

  val op < = lt

  val fresh_name = (0w0, "fresh")
  (* Keep track of used ids *)
  val next_id = ref 0w1

  fun hash (w, _) = w

  exception NOT_FOUND
  structure StringHash =
      HashTableFn (type hash_key = string
                   val  hashVal = HashString.hashString
                   val  sameKey = (op = : string * string -> bool))
  (* make must always return the same name when given the same
   * string. So we store the returned names in a hash table. *)
  val inputstring2name_map = StringHash.mkTable (37, NOT_FOUND)
      : name StringHash.hash_table
  (* In addition, for name stripping of ids and pp_unchanged functionality to
   * function properly, we must keep track of the actual strings asssociated
   * with names and the additional ids associated with a these *)
  val namestring2ids_map = StringHash.mkTable (38, NOT_FOUND)
      : word list StringHash.hash_table

  (* insert the special fresh name in the map *)
  fun reset () =
    (next_id := 0w1;
     StringHash.clear inputstring2name_map;
     StringHash.insert inputstring2name_map (#2 fresh_name, fresh_name);
     StringHash.clear namestring2ids_map;
     StringHash.insert namestring2ids_map (#2 fresh_name, [#1 fresh_name]))

  val () = reset ()

  fun fresh (SOME (n as (_, s))) =
      (case StringHash.find namestring2ids_map s of
         SOME ids =>
         let
           val id = !next_id
         in
           (next_id := !next_id + 0w1;
            StringHash.insert namestring2ids_map (s, id::ids);
            (id, s))
         end
       | NONE => n) (* the given name n must already be in namestring2ids_map,
                    * so this will not happen *) 
    | fresh NONE = fresh (SOME fresh_name)


  (* Utility function which strips off a trailing "_id" of a string
   * where id is a hex-digit. If nothing is stripped, NONE is returned.
   * Otherwise, SOME (s', id) is returned, where s' is the stripped string
   * and id is the hex-digit.
   * Note that strings consisting only of "_id" are not stripped.  *)
  fun strip_id s =
      let
        fun strip_id' 0 _    = NONE
          | strip_id' i last =
            let
              val c = String.sub (s, i)
            in
              if Char.isHexDigit c then
                strip_id' (i - 1) false
              else if c = #"_" andalso not last then
                SOME (String.substring (s, 0, i),
                      String.extract (s, i + 1, NONE))
              else
                NONE
            end
      in
        strip_id' (String.size s - 1) true
      end
  (* If present, a trailing "_id" is stripped off. *)
  fun strip s = case strip_id s of
                  NONE => s
                | SOME (s', _) => s'

  fun make s =
      case StringHash.find inputstring2name_map s of
        SOME n => n
      | NONE =>
        let
          val s' = if Flags.getBoolFlag "/kernel/bg/name/strip" then
                     strip s
                   else
                     s
          val id = !next_id
          val n = (id, s')
        in
          (next_id := !next_id + 0w1;
           StringHash.insert inputstring2name_map (s, n);
           (case StringHash.find namestring2ids_map s' of
              SOME ids => StringHash.insert namestring2ids_map (s', id::ids)
            | NONE     => StringHash.insert namestring2ids_map (s', [id]));
           n)
        end
  fun ekam ((_, s) : name) = s (*FIXME doesn't return the original name if strip = true*)
  fun unmk ((id, s) : name) =
      s ^ "_" ^ (String.map Char.toLower (Word.toString id))
    
  structure Order : ORDERING =
  struct 
    type T = name 
    fun lt n1 n2 = n1 < n2
  end

  structure NameSet = Rbset(type t = name val compare = compare)
  type nameset = NameSet.Set

  (* Utilities to keep track of which names should be printed specially: *)
  structure NameHash =
      HashTableFn (type hash_key = name
                   val  hashVal  = hash
                   val  sameKey  = ==)
  (*   maps 'problematic' names to similar unproblematic names. *)
  val pp_changed_map =
      NameHash.mkTable (37, NOT_FOUND) : name NameHash.hash_table
  (*   names which should be printed by using ekam instead of unmk. *)
  val pp_unchanged_names = ref NameSet.empty
  (*   map the original names (strings) of names in pp_unchanged_names
   *   to those names. *)
  val pp_unchanged_original_names
    = StringHash.mkTable (37, NOT_FOUND)
      : name StringHash.hash_table
  (*   names which should not be removed in a 'pp_unchanged' session *)
  val pp_unchanged_fixed = ref NameSet.empty

  fun pp indent pps n =
      if NameSet.member n (!pp_unchanged_names) then
        PrettyPrint.add_string pps (ekam n)
      else
        case NameHash.find pp_changed_map n of
          NONE    => PrettyPrint.add_string pps (unmk n)
        | SOME n' => PrettyPrint.add_string pps (unmk n')

  fun toString n
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (pp (Flags.getIntFlag "/misc/indent"))
        n

  exception PPUnchangedNameClash of name * name
  fun explain_PPUnchangedNameClash (PPUnchangedNameClash (n1, n2)) =
      Exp (LVL_USER, Origin.unknown_origin, pp_nothing,
           map (fn n => Exp (LVL_USER, Origin.unknown_origin,
                             pack_pp_with_data pp n, []))
               [n1, n2])
      :: [Exp (LVL_LOW, file_origin, pp_nothing, [])]
    | explain_PPUnchangedNameClash _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "names will clash if printed unchanged"
               explain_PPUnchangedNameClash)

  local
    (* add a name to the set of names that should be printed unchanged *)
    fun insert_pp_unchg_name (x as (id, s)) =
        (* First, check that it doesn't collide with another name that should
         * be printed unchanged. *)
        case StringHash.find pp_unchanged_original_names s of
          SOME (y as (id', _)) => if id = id' then
                                    () (* x is already in pp_unchanged*)
                                  else
                                    raise PPUnchangedNameClash (x, y)
        | NONE =>
          (* Second, record that x should be printed unchanged. *)
          (  pp_unchanged_names := NameSet.insert x (!pp_unchanged_names)
           ; StringHash.insert pp_unchanged_original_names (s, x)
          (* Third, eliminate potential clashes for the given name.
           * I.e. check whether the printing of the name without appending "_xx"
           * (where xx is its hex-digit id) clashes with a name n' in
           * namestring2ids_map when n' has its hex-digit id appended. If that is
           * the case, record that n' should be printed with another id. *)
           ; case strip_id s of
               NONE => ()
             | SOME (s', i) =>
            (case StringHash.find namestring2ids_map s' of
               NONE => ()
             | SOME ids =>
               let
                 val id = valOf (Word.fromString i)
               in
                 if List.exists (fn id' => id = id') ids then
                   (* create a new name to use instead of n' *)
                   let
                     val n' = (id, s')
                   in
                     (* FIXME ensure that the fresh name doesn't clash, i.e.
                      *       that it doesn't collide with a name in pp_unchanged *)
                     NameHash.insert pp_changed_map (n', fresh (SOME n'))
                   end
                 else
                   ()
               end))
    (* as the insert_pp_unchg_name, except that clashes are silently ignored *)
    fun insert_pp_unchg_name' x
      = (insert_pp_unchg_name x) handle PPUnchangedNameClash_ => ()

    (* remove a name from the set of names that should be printed unchanged *)
    fun remove_pp_unchg_name (x as (_, s)) =
        if NameSet.member x (!pp_unchanged_names)
           andalso not (NameSet.member x (!pp_unchanged_fixed)) then
          (  StringHash.remove pp_unchanged_original_names s
           ; pp_unchanged_names := NameSet.remove x (!pp_unchanged_names))
          handle NOT_FOUND => ()
        else ()

    fun clear () = (  NameHash.clear pp_changed_map
                    ; StringHash.clear pp_unchanged_original_names
                    ; pp_unchanged_names := NameSet.empty)
  in
    (*   register a new set of names to be printed without underscores. *)
    fun pp_unchanged X =
        (  clear ()
         ; pp_unchanged_fixed := X
         ; NameSet.apply insert_pp_unchg_name X)
        handle e => (clear (); raise e)

    (*   best effort to register an additional set of names to be printed
     *   without underscores. *)
    fun pp_unchanged_add X = NameSet.apply insert_pp_unchg_name' X
          
    (*   remove a set of names from the set of names that should be printed
     *   without underscores. *)
    fun pp_unchanged_remove X = NameSet.apply remove_pp_unchg_name X
  end


  val _ = Flags.makeBoolFlag
            {name = "/kernel/bg/name/strip",
             default = false,
             short = "", long = "strip-names",
             arg = "",
             desc = "Strip trailing _xx off input names (xx are hex digits)"}
end


functor Name (structure ErrorHandler : ERRORHANDLER
                where type ppstream    = PrettyPrint.ppstream
                  and type break_style = PrettyPrint.break_style
                  and type origin      = Origin.origin)
  :> NAME =
struct
  structure Name = Name'(structure ErrorHandler = ErrorHandler)
  open Name
end
