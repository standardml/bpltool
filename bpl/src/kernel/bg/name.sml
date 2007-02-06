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

  val fresh_name = (0w0, "__fresh")
  (* Keep track of used ids *)
  val next_id = ref 0w1

  (* Apparently the following hash function is advocated by
   * Knuth - at the very least it actually works in Moscow ML. *)
  fun stringhash s = 
      let
        open Word
        fun f (c,h) = 
            xorb(xorb(<<(h,0w5),>>(h,0w27)), fromInt (ord c))
      in
        CharVector.foldr f 0w0 s
      end

  fun hash (w, _) = w

  (* make must always return the same name when given the same
   * string. So we store the returned names in a hash table.
   * In addition, for the pp_unchanged functionality to function
   * properly, we must keep track of additional ids associated
   * with a string, thus we also store a list of ids. *)
  exception NOT_FOUND
  structure StringHash =
      HashTableFn (type hash_key = string
                   val  hashVal = stringhash
                   val  sameKey = (op = : string * string -> bool))
  val name_map = StringHash.mkTable (37, NOT_FOUND)
      : (name * word list) StringHash.hash_table

  (* insert the special fresh name in the map *)
  val () = StringHash.insert name_map ("__fresh", (fresh_name, []))

  fun fresh (SOME (n as (_, s))) =
      (case StringHash.find name_map s of
         SOME (n, ids) =>
         let
           val id' = !next_id
         in
           (next_id := !next_id + 0w1;
            StringHash.insert name_map (s, (n, id'::ids));
            (id', s))
         end
       | NONE => n) (* the given name n must already be in name_map,
                    * so this will not happen *) 
    | fresh NONE = fresh (SOME fresh_name)


  fun make s =
      case StringHash.find name_map s of
        SOME (n, _) => n
      | NONE =>
        let
          val n = (!next_id, s)
        in
          (next_id := !next_id + 0w1;
           StringHash.insert name_map (s, (n, []));
           n)
        end
  fun ekam ((_, s) : name) = s
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

  exception PPUnchangedNameClash of name * name
  (* the explainer uses the prettyprinter, so it is placed further down... *)

  (*   check whether any of the name-strings in the given nameset clashes. *)
  fun name_string_clashes X =
      let
        val map = StringHash.mkTable (NameSet.size X, NOT_FOUND)
          : name StringHash.hash_table
        fun insert_name (x as (_, s)) =
            case StringHash.find map s of
              SOME y => raise PPUnchangedNameClash (x, y)
            | NONE   => StringHash.insert map (s, x)
      in
        NameSet.apply insert_name X
      end
  (*   register a new set of names to be printed without underscores. *)
  fun pp_unchanged X Y =
      let
        val () = NameHash.clear pp_changed_map
        val ()
          = (name_string_clashes X; name_string_clashes Y)
            handle e => (pp_unchanged_names := NameSet.empty; raise e)
        val () = pp_unchanged_names := NameSet.union' X Y

        (* check potential clashes for the given name. *)
        fun check_name (w_a, s_a) =
            let
              (* Split the name-string at the last "_" *)
              val (s, i, _, _)
                = case String.fields (fn c => c = #"_") s_a of
                    []  => ("", "", false, false)
                  | [s] => (s,  "", false, false)
                  | ss 
                    => foldr (fn (i, (_, _, _, true)) =>
                                 ("", i, true, false)
                               | (s, (ss, i, last, false)) =>
                                 let
                                   val ss' = if last then s else s^"_"^ss
                                 in
                                   (ss', i, false, false)
                                 end)
                             ("", "", true, true) ss
              (* we must check "manually" that the string is a number,
                 since fromString will happily parse a number-string
                 with garbage at the end...
                 Also, numbers with initial zeros are not relevant. *)
              fun isNumber' s j l =
                  j >= l orelse (Char.isDigit (String.sub (s, j))
                                 andalso isNumber' s (j + 1) l)
              fun isNumber s =
                  let
                    val l = String.size s
                  in
                    l > 0 andalso String.sub (s, 0) <> #"0"
                    andalso isNumber' s 0 l
                  end
            in
              (* is the last part a number?
                 and does the first part clash with another name
                 which uses that number as id? *)
              if isNumber i then
                case StringHash.find name_map s of
                  NONE => ()
                | SOME (c as (id', _), id's) =>
                  let
                    val id = valOf (Word.fromString i)
                  in
                    if id = id'
                         orelse not (List.all (fn id' => id' <> id) id's) then
                      (* create a new name to use instead of c *)
                      NameHash.insert pp_changed_map ((id, s), fresh (SOME c))
                    else
                      ()
                  end
              else ()
            end
      in
        NameSet.apply check_name (!pp_unchanged_names)
      end
 
 
  fun pp indent pps n =
      if NameSet.member n (!pp_unchanged_names) then
        PrettyPrint.add_string pps (ekam n)
      else
        case NameHash.find pp_changed_map n of
          NONE => PrettyPrint.add_string pps (unmk n)
        | SOME n' => PrettyPrint.add_string pps (unmk n')


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
