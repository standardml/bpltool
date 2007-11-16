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

(** Prettyprinting bigraph binding discrete normal forms (BDNF) as 
 * Scalable Vector Graphics (SVG).
 * @version $LastChangedRevision: 922 $
 *)
signature PPSVG =
sig
  type B
  type 'class bgbdnf
  (** A representation of location-dependent configuration data. *)
  type config
  (** Syntax tree location type.  @see mkconfig. *)
  type path = int list
  (** Configuration parameters. *)
  type configinfo = {
    (** Horizontal space around nodes. *)
      xsep           : int,
    (** Vertical space around nodes. *)
      ysep           : int,
    (** Height of the font used for control names. *)
      ctrlfontheight : int,
    (** Width of the characters used for control names. *)
      ctrlcharwidth  : int,
    (** Height of the font used for names. *)
      namefontheight : int,
    (** Width of the characters used for names. *)
      namecharwidth  : int,
    (** Height of the font used for numbering the sites. *)
      sitefontheight : int,
    (** Width of the characters used for numbering the sites. *)
      sitecharwidth  : int,
    (** Horizontal space between control label and first free port. *)
      textmargin     : int,
    (** Vertical space below text. *)
      textysep       : int,
    (** Length of single-point edges. *)
      idleedgelength : int,
    (** Minimum width of a root. *)
      rootwidth      : int,
    (** Minimum height of a root. *)
      rootheight     : int,
    (** Root corner rounding radius. *) 
      rootrounding   : int,
    (** Minimun width of a node. *)
      nodeminwidth   : int,
    (** Minimum height of a node. *)
      nodeminheight  : int,
    (** Horizontal spacing between node ports. *)
      portsep        : int,
    (** Radius of binders. *)
      binderradius   : int,
    (** Minimum widht of a site. *)
      sitewidth      : int,
    (** Minimum height of a site. *)
      siteheight     : int,
    (** Site corner rounding radius. *) 
      siterounding   : int
  }
  (** Construct a config from a function.  The function must
   * take a string and a path.  The string indicates the current node:
   * root="", node=control name, site=site number, and the path
   * indicates an exact location in the tree: [2, 3, 0] is the
   * third child node (or site) of the fourth child node of the
   * first root -- note that numbering begins at zero.
   * The function must return the desired configuration parameters
   * for the indicated node at the indicated location.
   *) 
  val makeconfig : ((string * path) -> configinfo) -> config
  (** Deconstruct config. *)
  val unmkconfig : config -> ((string * path) -> configinfo)
  (** Default config. *)
  val defaultconfig : config
  (** Prettyprint a bgbdnf, producing SVG. *)
  val ppsvg : config option -> B bgbdnf -> string
  (** Prettyprint a bgbdnf, producing an XML document including
   * preamble, styling and SVG.
   *)
  val ppsvgdoc : config option -> B bgbdnf -> string
end
