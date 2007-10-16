(* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
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

(** Callback functions for SAX parsing a BPL signature expressed in XML.
 * @version $LastChangedRevision: 442 $
 * Modified: $Date: 2006/09/04 21:48:46 $ by: $Author: hniss $
 *)

functor SignatureXMLHooks (structure Control : CONTROL) :> BPLXMLHOOKS
	where type initDatatype = unit
	  and type resulttype = Control.control list =
struct
  structure StringMap =
    OrderFinMap (struct type T = string fun lt x (y : string) = x < y end)
  type 'a stringmap = 'a StringMap.map
  structure IntMap =
    OrderFinMap (struct type T = int fun lt x y = x < y end)
  type 'a intmap = 'a IntMap.map
  open IgnoreHooks
  datatype part = NOTHING | TYPEMAP | TYPE | ROOT
  type sigdata = {
  	(* typemap maps control types to (control name, control kind) *)
    typemap : (string * string option) stringmap,
    (* types is a list of (control types, bound arity, free arity) *)
    types : (string * int * int) list,
    (* part signals the current location within the XML schema *)
    part : part 
  }
  fun partToString TYPEMAP = "TYPEMAP"
    | partToString TYPE    = "TYPE"
    | partToString NOTHING = "NOTHING"
    | partToString ROOT    = "ROOT"
	(* An element index handler takes an element index, the list of
	 * attributes, signature data and returns the signature data
	 * updated with any information extracted from the element and
	 * attributes.
	 *) 
  type handlers =
    ((HookData.StartEnd -> HookData.AttSpecList -> sigdata -> sigdata) *
     (HookData.StartEnd -> sigdata -> sigdata)) intmap
  type AppData = Dtd.Dtd * handlers * sigdata
  type AppFinal = Control.control list
  type initDatatype = unit
  type resulttype = Control.control list
  exception ExpectedNameType of HookData.StartEnd
  exception ExpectedName of HookData.StartEnd
  exception UndeclaredControlType of string
  exception ExpectedAttribute of HookData.StartEnd
  fun getAttVal pos (HookData.AP_DEFAULT (_, value, _))
    = UniChar.Vector2String value
    | getAttVal pos (HookData.AP_PRESENT (_, value, _))
    = UniChar.Vector2String value
    | getAttVal pos _ = raise ExpectedAttribute pos
  fun init (_, dtd) = 
    let
		  (* The handleXXX functions each handle an XML element with tag XXX.
		   * Given the list of attributes and signature data, the latter is
		   * returned, updated with any information extracted from the element.
		   *)
		  fun handleGroup _ _ (sigdata as {typemap, types, part}) =
		    case part of
		      NOTHING => {typemap = typemap, types = types, part = TYPEMAP}
		    | _ => sigdata
		  fun handleElement pos atts (sigdata as {typemap, types, part}) =
		    case part of
		      TYPEMAP => 
		      let
		        fun extractdata ((idx, value, _) :: atts)
		              (ntk as (name, typ, kind))
		          = let
		              val value = getAttVal pos value
		            in
		              case UniChar.Data2String (Dtd.Index2AttNot dtd idx) of
		                "name" => extractdata atts (SOME value, typ, kind)
		              | "type" => extractdata atts (name, SOME value, kind)
		              | "kind" => extractdata atts (name, typ, SOME value)
		              | _ => extractdata atts (name, typ, kind)
		            end
		          | extractdata _ ntk = ntk
		      in
		        case extractdata atts (NONE, NONE, NONE) of
		          (SOME "site", _, _) => sigdata
		        | (SOME name, SOME typ, kind) =>
		          {typemap = StringMap.add (typ, (name, kind), typemap),
		           types = types,
		           part = part}
		        | _ => raise ExpectedNameType pos 
		      end
		    | NOTHING => {typemap = typemap, types = types, part = ROOT}
		    | _ => sigdata
		  fun findname pos ((idx, value, _) :: atts) =
		      if UniChar.Data2String (Dtd.Index2AttNot dtd idx) = "name" then
		        getAttVal pos value
		      else
		        findname pos atts
		    | findname pos _ = raise ExpectedName pos
		  fun handleComplexType pos atts (sigdata as {typemap, types, part}) =
		    case part of
		      NOTHING =>
		      {typemap = typemap,
		       types = (findname pos atts, 0, 0) :: types,
		       part = TYPE}
		    | _ => sigdata
		  fun handleAttribute pos atts (sigdata as {typemap, types, part}) =
(print ("State(" ^ partToString part ^ ")\n"); 
		    case (part, types) of
		      (TYPE, (name, bound, free) :: typs) =>
		      {typemap = typemap,
		       types =
		        (if String.sub (findname pos atts, 0) = #"b" then
		           (name, bound + 1, free)
		         else
		           (name, bound, free + 1)) :: typs,
		       part = part}
		     | _ => sigdata)
		  fun handleEndGroup _ {typemap, types, part = TYPEMAP}
		    = {typemap = typemap, types = types, part = NOTHING}
		    | handleEndGroup _ ttp = ttp
		  fun handleEndElement _ {typemap, types, part = ROOT}
		    = {typemap = typemap, types = types, part = NOTHING}
		    | handleEndElement _ ttp = ttp
		  fun handleEndComplexType _ {typemap, types, part = TYPE}
		    = {typemap = typemap, types = types, part = NOTHING}
		    | handleEndComplexType _ ttp = ttp
		  fun handleEndAttribute _ ttp = ttp
		  fun addhandler ((tag, starthandler, endhandler), handlers) =
        let
          val idx = Dtd.Element2Index dtd (UniChar.String2Data tag)
        in
          IntMap.add (idx, (starthandler, endhandler), handlers)
        end
    	val handlers =
    	  foldr addhandler IntMap.empty
    		 [("xs:group", handleGroup, handleEndGroup),
    		  ("xs:element", handleElement, handleEndElement),
    		  ("xs:complexType", handleComplexType, handleEndComplexType),
    		  ("xs:attribute", handleAttribute, handleEndAttribute)]
      val initsigdata = {
        typemap = StringMap.empty,
        types = [],
        part = NOTHING}
    in
      (dtd, handlers, initsigdata)
    end
  fun getResult cs = cs
  fun hookFinish (_, _, {typemap, types, part}) =
    let
      fun mkkind (SOME "active")  = Control.Active
        | mkkind (SOME "passive") = Control.Passive
        | mkkind (SOME "atomic")  = Control.Atomic
        | mkkind _                = Control.Active
      fun mkControl (typename, bound, free) =
        case StringMap.lookup typemap typename of
        NONE => raise UndeclaredControlType typename
      | SOME (name, kind) =>
        Control.make (name, mkkind kind, bound, free)
    in
      map mkControl (rev types)
    end
  fun hookError (appData, (errpos, err)) =
    (print
     (Errors.Position2String errpos ^ ": " ^
      concat (Errors.errorMessage err) ^ "\n");
     appData)
  fun hookStartTag
    ((dtd, handlers, sigdata), (pos as (startpos, endpos), elemIdx, atts, spaces, empty)) =
    let
      open HookData
      val pos2str = ErrorString.Position2String
      fun s2str NONE = "<*>"
        | s2str (SOME (x1, x2))
        = UniChar.Data2String x1 ^ " " ^ UniChar.Data2String x2
      fun attval2str NONE = "(*)"
        | attval2str (SOME v) = "(...)"
      fun att2str (i, AP_IMPLIED, s) = Int.toString i ^ " -- " ^ s2str s
        | att2str (i, AP_MISSING, s) = Int.toString i ^ " - " ^ s2str s
        | att2str (i, AP_DEFAULT (v1, v2, attval), s)
        = Int.toString i ^ "(default):" ^ UniChar.Vector2String v1 ^
          UniChar.Vector2String v2 ^ attval2str attval ^ " - " ^ 
          s2str s
        | att2str (i, AP_PRESENT (v1, v2, attval), s)
        = Int.toString i ^ "(present):" ^ UniChar.Vector2String v1 ^
          UniChar.Vector2String v2 ^ attval2str attval ^ " - " ^ 
          s2str s
      fun atts2str [] = ""
        | atts2str (x :: x' :: xs) = att2str x ^ ", " ^ atts2str (x' :: xs)
        | atts2str [x] = att2str x
      fun elemIdx2str idx = UniChar.Data2String (Dtd.Index2Element dtd idx)
    in
      print
        (pos2str startpos ^ "-" ^ pos2str endpos ^ "[" ^
         Int.toString elemIdx ^ "-" ^ elemIdx2str elemIdx ^
         "]: [" ^ atts2str atts ^ "]<" ^
         UniChar.Data2String spaces ^
         (if empty then "/>" else ">") ^ "\n");
      case IntMap.lookup handlers elemIdx of
        SOME (handler, _) => (dtd, handlers, handler pos atts sigdata)
      | NONE => (dtd, handlers, sigdata)
    end
  fun hookEndTag
    ((dtd, handlers, sigdata),
     (pos, elemIdx, _)) =
    case IntMap.lookup handlers elemIdx of
      SOME (_, handler) => (dtd, handlers, handler pos sigdata)
    | NONE => (dtd, handlers, sigdata)
end
