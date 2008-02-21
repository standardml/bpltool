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

(** Main BG module, including file I/O and parsing.  For just the
 * BPL data types, see the BG_ADT module.
 * @version $LastChangedRevision$
 *)
signature BG =
sig

  include BG_ADT

  type ruledata = {
    name : string,
    redex : bgterm,
    react : bgterm,
    maps : ((int * Name.name list) * (int * Name.name list)) list,
    info : Info.info}
  (** The kind of contents to parse. *)
  type 'a kind
  (** Signature contents. *)
  val SIGNATURE : control list kind
  (** BgTerm contents. *)
  val BGTERM : bgterm kind
  (** List of rules contents. *)
  val RULES : ruledata list kind

  (** Parse a string as a bigraph term, using the old syntax.
   * The following grammar is used, with where the regexps for
   * keywords are CTRLID = [A-Z?!][A-Za-z0-9_]* and
   * ID = [a-z][A-Za-z0-9_]*
   * <pre>
bg              ::= altbg
                 |  bg * bg
                 |  `**[ bglist ]
                 |  bg | bg
                 |  bg || bg
                 |  bg o bg

bglist          ::= bg , bglist
                 |  bg
                 |

altbg           ::= ( )
                 |  "idx_0"
                 |  "id||_0"
                 |  "merge_" N
                 |  "1"
                 |  ' nset '
                 |  link
                 |  "idw_" nset
                 |  "idw_0"
                 |  CTRLID < nlist > < nsetlist >
                 |  CTRLID < nlist >
                 |  CTRLID
                 |  [ permentrylist ]
                 |  "idp_" N
                 |  ( nset ) bg
                 |  ( bg )
                 |  ///[ wiringentrylist ]

nlist           ::= ID , nlist
                 |  ID
                 |

nsetlist        ::= nset , nsetlist
                 |  nset
                 |

nset            ::= { nlist }

link            ::= ID / ID
                 |  ID / nset
                 |  / ID
                 |  / nset

permentrylist   ::= permentry , permentrylist
                 |  permentry
                 |

permentry       ::= INT nset
                 |  INT

wiringentrylist ::= link , wiringentrylist
                 |  link
                 |
   * </pre>
   * @params filename s
   * @param filename  File name to use when reporting errors.
   * @param s         The string to be parsed.
   *)
  val parseBgTermStr : string -> string -> bgterm
  (** Parse a string as a kind of contents.
   * The following grammar is used, with where the regexps for
   * keywords are CTRLID = [A-Z?!][A-Za-z0-9_]* and
   * ID = [a-z][A-Za-z0-9_]*
   * <pre>
start         ::= bg                ; Bigraph
               |  rulelist          ; List of rules
               |  ctrllist          ; Signature

ctrllist      ::= [ ctrls ]
               |  [ ]

ctrls         ::= ctrl
               |  ctrl , ctrls

ctrl          ::= kind ( cdata )

kind          ::= "active"  | "active0"
               |  "passive" | "passive0"
               |  "atomic"  | "atomic0"

cdata         ::= CTRLID
               |  CTRLID -: INT
               |  CTRLID =: INT --> INT

rulelist      ::= [ rules ]
               |  [ ]
         
rules         ::= rule
               |  rule , rules

rule          ::= [ rulefields ]

rulefields    ::= rulefield
               |  rulefield , rulefields

rulefield     ::= "redex" = bg
               |  "react" = bg
               |  "inst"  = inst
               |  "name"  = STRING

inst          ::= [ maps ]
               |  [ ]

maps          ::= map
               |  map , maps

map           ::= INT |-> INT
               |  INT & nset |--> INT & nset

bg            ::= altbg
               |  bg  *  bg           ; Tensor product
               |  bg `|` bg           ; Prime product
               |  bg  || bg           ; Parallel product
               |  bg  o  bg           ; Composition

altbg         ::= <->                 ; Barren root
               |  idx0                ; Empty tensor product
               |  id||0               ; Empty parallel product
               |  "merge" pint        ; Merge
               |  ` nset `            ; Concretion
               |  link                ; Wiring
               | "idw" nset           ; Identity wiring
               | "idw0"               ; Empty wiring
               | CTRLID fports bports ; Binding ion
               | CTRLID fports        ; Ion
               | CTRLID               ; Portless ion
               | [ permentrylist ]    ; Permutation
               | "idp" pint           ; Identity permutation
               | < nset > bg          ; Abstraction
               | ( bg )
               | << bg >>             ; Hole puncher for atomic ions

fports	      ::= [ names ]

names         ::= ID , names
               |  ID
               |

bports	      ::= [ nsetlist ]

nsetlist      ::= nset , nsetlist
	       |  nset
               |

nset          ::= [ names ]

link	      ::= ID /  ID           ; Renaming
               |  ID // nset         ; Substitution
 	       |  -/  ID             ; Closure
               |  -// nset           ; Multiple closures

permentrylist ::= permentry , permentrylist
	       |  permentry
	       |

permentry     ::= INT & nset
	       |  INT

pint          ::= INT
               |  ( pint )
   * </pre>
   * @params kind filename s
   * @param kind      The kind of contents to expect when parsing.
   * @param filename  File name to use when reporting errors.
   * @param s         The string to be parsed.
   *)
  val parseStr : 'a kind -> string -> string -> 'a
  (** Read a BG expression from a file print it, and return it as BDNF. *)
  val usefile : string -> B bgbdnf
  (** Read a BG expression from a file, return it as BDNF, explain
   * any errors on stdOut.
   *)
  val usefile' : string -> B bgbdnf
  (** Read a BG expression from a file, return it as BDNF. *)
  val usefile'' : string -> B bgbdnf
  (** Read a BG expression, using old syntax, from a file, return it as BDNF. *)
  val useBgTermfile'' : string -> B bgbdnf
  (** Read a BG expression from a file, return it as a bgval, explain
   * any errors on stdOut.
   *)
  val bgvalUsefile' : string -> bgval
  (** Read a BG expression from a file, return it as a bgval. *)
  val bgvalUsefile'' : string -> bgval
  (** Read a BG expression, using old syntax, from a file, return it as a bgval. *)
  val bgvalUseBgTermfile'' : string -> bgval
  (** ADDED - necessary? *)
  val bgtermUsefile'' : string -> bgterm			    

  (** Read a BPL program (a BRS consisting of agent and rules) from
   * a file, return it as a bgval and a list of rules.
   *)
  val brsUseBPLTermFile : string -> bgval * rule list

  (** Read a bigraphical signature from one XML file and a BRS from
   * another XML file. *)
  val brsUseXMLfiles : string -> string -> control list * ruledata list
  (** Prettyprinter for bigraphs. *)
  val pp : PrettyPrint.ppstream -> 'class bgbdnf -> unit
  (** Return string representation of a normal form bigraph. *)
  val toString : 'class bgbdnf -> string
  (** Return string representation of a bigraph value. *)
  val bgvalToString : bgval -> string

  structure PPSVG : PPSVG
  
  sharing type BgBDNF.bgbdnf = PPSVG.bgbdnf
  sharing type BgBDNF.B = PPSVG.B
  (** Return an SVG representation of a bigraph. *)
  val toSVGString : PPSVG.config option -> B bgbdnf -> string
  (** Return an SVG representation of a bigraph. *)
  val bgvalToSVGString : PPSVG.config option -> bgval -> string

  structure BgTermParser : PARSER
  
  structure RulesParser : PARSER

end
