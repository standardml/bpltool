(* -*-sml-*-mode *)
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

(** A lexer for the PEPA representation of bigraphs.
 * @version $LastChangedRevision: 102 $
 *)

structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err (p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

(* If true; output tokens while reading *)
val debug = ref false;

fun debugprint str = 
if (!debug) then print(str^"\n") else ()

fun debugprintpos leftpos rightpos str =
    if (!debug) then
	 print (Int.toString leftpos ^ "-" 
		^ Int.toString rightpos ^ " " ^ str ^ "\n")
    else
      ()

(* This is necessary to reset line-counters between several parses... *)
  (* val () = ErrorMsg.reset(); *)

   structure H = HashTable
   
   exception KWNotFound
   exception SignNotFound

   type lookup_table = (string, pos * pos -> lexresult) H.hash_table
   
val kw_table : lookup_table = 
   H.mkTable(HashString.hashString, op = ) (32,KWNotFound)

val sign_table : lookup_table = 
   H.mkTable(HashString.hashString, op = ) (32,SignNotFound)

val () =
   List.app (fn (str,tok) => H.insert kw_table (str, tok))
     [
      ("Node",    Tokens.NODE),
      ("Edge",    Tokens.EDGE),
      ("Name",    Tokens.NAME),
      ("Root",    Tokens.ROOT),
      ("ctrl",    Tokens.CTRL),
      ("prnt",    Tokens.PRNT),
      ("port",    Tokens.PORT),
      ("Present", Tokens.PRESENT),
      ("Absent",  Tokens.ABSENT),
      ("<*>",     Tokens.COLLABSTAR),
      ("__",      Tokens.UUSCORE)
      ]

val () =
   List.app (fn (str,tok) => H.insert sign_table (str, tok))
     [
      ("{", Tokens.LBRACE),
      ("}", Tokens.RBRACE)
      ]

local
   fun lookup(str, leftpos, rightpos, table, noneAct) =
   case H.find table str
   of SOME tok     => (debugprintpos leftpos rightpos ("TOKEN: " ^ str); 
		       tok (leftpos, rightpos))
     |NONE         =>  noneAct()
in
   fun lookupKeyword (str, leftpos, rightpos) = 
     lookup (str, leftpos, rightpos, kw_table, 
	    (fn()=> (debugprintpos leftpos rightpos ("ID("^str^")");
		     Tokens.ID (str, leftpos, rightpos))))

   fun lookupSign (str, leftpos, rightpos) = 
     lookup(str, leftpos, rightpos, sign_table, 
	    (fn() => raise SignNotFound)) 
end

(* for simple comment-handling *)
val comlevel = ref 0;

%%
%header (functor PepaBgLex(structure Tokens : PepaBg_TOKENS));
KW_ID      = "<*>" | "__" | [a-zA-Z](_?[A-Za-z0-9'])*;
INT        = (0 | [1-9][0-9]*);
WHITESPACE = [\ \t\013];
%%
\n             => ( lineNum := !lineNum+1; 
                      linePos := yypos :: !linePos; continue());
{WHITESPACE}+  => ( continue() );
{KW_ID}        => ( if (!comlevel > 0) then
                      continue() 
                    else 
                      lookupKeyword (yytext, 
                                     yypos,
                                     yypos + size yytext));
{INT}          => ( if (!comlevel > 0) then
                      continue()
                    else
                      (debugprintpos yypos  
                                     (yypos + size yytext)
                                     ("INT("^yytext^")");
                       Tokens.INT (valOf(Int.fromString yytext),
                                   yypos,
                                   yypos + size yytext)));
(.)            => ( if (!comlevel > 0) then continue()
                    else lookupSign (yytext, yypos, yypos)
                         handle SignNotFound =>
                                (ErrorMsg.error 
                                     yypos (yypos + 1)
                                     (" illegal character : ASCII " 
                                      ^ (Int.toString o ord o hd o explode)
                                            yytext 
                                            ^ " '" ^ yytext ^ "'" );
                                 continue()));
