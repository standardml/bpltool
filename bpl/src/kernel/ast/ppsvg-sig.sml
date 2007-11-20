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
 *
 <svg version='1.1'
  xmlns='http://www.w3.org/2000/svg' fill='none' stroke='black'>
  <rect class='root' x='0' y='16' rx='5' ry='5' width='187' height='119' />
  <ellipse class='node' cx='93' cy='75' rx='88' ry='54' />
  <text class='node' x='5' y='48'>K</text>
  <ellipse class='node' cx='61' cy='79' rx='30' ry='32' />
  <text class='node' x='31' y='50'>L</text>
  <rect class='site' x='41' y='68' rx='5' ry='5' width='40' height='30' />
  <text class='site' x='41' y='65'>0</text>
  <text class='name' x='60' y='81' text-anchor='middle'>x</text>
  <ellipse class='node' cx='126' cy='79' rx='30' ry='32' />
  <text class='node' x='96' y='50'>M</text>
  <rect class='site' x='106' y='68' rx='5' ry='5' width='40' height='30' />
  <text class='site' x='106' y='65'>1</text>
  <path class='link' d='
    M60,68
    C60,44 86,33 86,13' />
  <path class='link' d='
    M113,50
    C113,26 86,33 86,13' />
  <text class='name' x='86' y='10' text-anchor='middle'>y</text>
</svg>
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
  (** SVG path data type. *)
  datatype pathdata
  = MoveTo    of (int * int) list
  | LineTo    of (int * int) list
  | HLineTo   of int list
  | VLineTo   of int list
  | CurveTo   of ((int * int) * (int * int) * (int * int)) list
  | SCurveTo  of ((int * int) * (int * int)) list
  | QCurveTo  of ((int * int) * (int * int)) list
  | SQCurveTo of (int * int) list
  | ClosePath
  
  (** SVG data type. *)
  datatype svg
  = Svg       of svg list
  | Text      of {
      class : string,  x : int,  y : int, text : string, anchor : string}
  | Rectangle of {
      class : string,  x : int,  y : int, r : int,
      width : int, height : int}
  | Circle    of {class : string, cx : int, cy : int, r : int}
  | Ellipse   of {class : string, cx : int, cy : int, rx : int, ry : int}
  | Path      of {class : string, d : pathdata list}

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
  (** Prettyprint a bgbdnf, producing SVG data.
   * @params config b
   * @param config  Config to use when prettyprinting.
   * @param b       Bigraph to prettyprint.
   *)
  val makesvg : config option -> B bgbdnf -> svg
  (** Prettyprint a bgbdnf, producing SVG.
   * @params ns config b
   * @param ns      Namespace prefix (e.g., "svg:").
   * @param config  Config to use when prettyprinting.
   * @param b       Bigraph to prettyprint.
   *)
  val ppsvg : string -> config option -> B bgbdnf -> string
  (** Prettyprint a bgbdnf, producing an XML document including
   * preamble, styling and SVG.
   * @params config b
   * @param config  Config to use when prettyprinting.
   * @param b       Bigraph to prettyprint.
   *)
  val ppsvgdoc : config option -> B bgbdnf -> string
  (** Prettyprint a bgbdnf, producing an XHTML document including
   * preamble, styling and SVG.
   * @params title config b
   * @param title   The page title
   * @param config  Config to use when prettyprinting.
   * @param b       Bigraph to prettyprint.
   *)
  val ppxhtmldoc : string -> config option -> B bgbdnf -> string
end
