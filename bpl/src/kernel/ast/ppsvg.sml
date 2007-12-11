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
 * Scalable Vector Graphics (SVG) using a naïve, but simple to implement
 * algorithm.  It passes over the tree twice: first to calculate widths
 * and heights from the inside out, then to draw the actual elements.
 * @version $LastChangedRevision: 922 $
 *)
functor PPSVG (
  structure Name        : NAME
  structure NameSet     : MONO_SET
  structure Interface   : INTERFACE
  structure Control     : CONTROL
  structure Ion         : ION
  structure Permutation : PERMUTATION
  structure Link        : LINK
  structure LinkSet     : MONO_SET
  structure Wiring      : WIRING
  structure BgVal       : BGVAL
  structure BgBDNF      : BGBDNF
  sharing type Name.name = Ion.name
                         = Link.name
                         = NameSet.elt
  sharing type NameSet.Set = Interface.nameset
                           = Link.nameset
                           = Ion.nameset
                           = Wiring.nameset
  sharing type Interface.interface = BgBDNF.interface
  sharing type Control.control = Ion.control
  sharing type Ion.ion = BgBDNF.ion
  sharing type Permutation.permutation = BgVal.permutation
  sharing type Wiring.wiring = BgBDNF.wiring
                             = BgVal.wiring
  sharing type LinkSet.elt = Link.link
  sharing type LinkSet.Set = Wiring.linkset
  sharing type BgVal.bgval = BgBDNF.bgval) :> PPSVG 
  where type B = BgBDNF.B
    and type 'a bgbdnf = 'a BgBDNF.bgbdnf =
struct
  type B = BgBDNF.B
  type 'class bgbdnf = 'class BgBDNF.bgbdnf
  structure Geometry2D =
    Geometry2D (
      type num = real
      val zero = 0.0
      fun real n = n
      fun fromReal n = n
      val compare = Real.compare
      val (op --) = Real.-
      val (op ++) = Real.+
      val (op **) = Real.*
      val (op ==) = Real.==)
  datatype direction = NW | NE | SW | SE | AUTO
  type path = int list
  type configinfo = {
      xsep           : int,
      ysep           : int,
      ctrlfontheight : int,
      ctrlcharwidth  : int,
      namefontheight : int,
      namecharwidth  : int,
      sitefontheight : int,
      sitecharwidth  : int,
      textmargin     : int,
      textysep       : int,
      idleedgelength : int,
      rootwidth      : int,
      rootheight     : int,
      rootrounding   : int,
      nodeminwidth   : int,
      nodeminheight  : int,
      labelpos       : direction,
      portsep        : int,
      binderradius   : int,
      sitewidth      : int,
      siteheight     : int,
      siterounding   : int
  }
  type config = (string * path) -> configinfo

  structure NameMap =
    OrderFinMap (type T = Ion.name fun lt x y = Name.< (x, y))

  type ltype =       (* Type of link information tuples:        *)
    int *            (* average x position of link points       *)
    int *            (* max x distance between link points      *)
    int *            (* unique edge number                      *)
    int *            (* link name length                        *)
    bool *           (* bound link flag                         *)
    string *         (* link name                               *)
    int *            (* minimum allowable x position for binder *)
    int *            (* maximum allowable x position for binder *)
    int *            (* minimum y position of link points       *)
    (int * int) list (* (x, y) link points coordinates          *)
  structure LSet =
    OrderSet (
      type T = ltype
      fun lt ((x1, dx1, u1, _, _, n1, _, _, _, _) : T)
             ((x2, dx2, u2, _, _, n2, _, _, _, _) : T) = 
        case (n1, n2) of
          ("", "") => (
          case Int.compare (dx1, dx2) of
            LESS => false
          | GREATER => true
          | EQUAL => (
            case Int.compare (x1, x2) of
              LESS => false
            | GREATER => true
            | EQUAL => u1 > u2))
        | ("", _) => false
        | (_, "") => true
        | _ =>
          case Int.compare (x1, x2) of
            LESS => false
          | GREATER => true
          | EQUAL => n1 > n2)

  structure EdgeSet =
    OrderSet (
      type T =
        int * int * int * int * bool * string *
        int * int * int * int * (int * int) list
      fun lt ((_, dx1, u1, _, _, _, _, _, _, y1, _) : T)
             ((_, dx2, u2, _, _, _, _, _, _, y2, _) : T) =
        case Int.compare (y1, y2) of
          LESS => true
        | GREATER => false
        | EQUAL =>
          case Int.compare (dx1, dx2) of
            LESS => false
          | GREATER => true
          | EQUAL => u1 < u2)

  fun intToString i = if i < 0 then "-" ^ Int.toString (~i) else Int.toString i
  
  (* SVG data types ************************************************)
  
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

  local
    open Int
  in
    (* Find the bounding box of two boxes. *)
    fun maxpair (((x1, y1), (x2, y2)), ((x1', y1'), (x2', y2')))
      = ((min (x1, x1'), min (y1, y1')),
         (max (x2, x2'), max (y2, y2')))
    (* Find the bounding box of a path that starts within a box xyxy. *)
    fun svgpathsize (MoveTo [], xyxy) = xyxy
      | svgpathsize (MoveTo (xy :: xys), _)
      = foldl maxpair (xy, xy) (map (fn xy => (xy, xy)) xys)
      | svgpathsize (LineTo [], xyxy) = xyxy
      | svgpathsize (LineTo (xy :: xys), _)
      = foldl maxpair (xy, xy) (map (fn xy => (xy, xy)) xys)
      | svgpathsize (HLineTo [], xyxy) = xyxy
      | svgpathsize (HLineTo (x :: xs), ((_, y1), (_, y2)))
      = ((foldl min x xs, y1), (foldl max x xs, y2))
      | svgpathsize (VLineTo [], xyxy) = xyxy
      | svgpathsize (VLineTo (y :: ys), ((x1, _), (x2, _)))
      = ((x1, foldl min y ys), (x2, foldl max y ys))
      | svgpathsize (CurveTo [], xyxy) = xyxy
      | svgpathsize (CurveTo ((_, _, xy) :: xys), _)
      = foldl maxpair (xy, xy) (map (fn (_, _, xy) => (xy, xy)) xys)
      | svgpathsize (SCurveTo [], xyxy) = xyxy
      | svgpathsize (SCurveTo ((_, xy) :: xys), _)
      = foldl maxpair (xy, xy) (map (fn (_, xy) => (xy, xy)) xys)
      | svgpathsize (QCurveTo [], xyxy) = xyxy
      | svgpathsize (QCurveTo ((_, xy) :: xys), _)
      = foldl maxpair (xy, xy) (map (fn (_, xy) => (xy, xy)) xys)
      | svgpathsize (SQCurveTo [], xyxy) = xyxy
      | svgpathsize (SQCurveTo (xy :: xys), _)
      = foldl maxpair (xy, xy) (map (fn xy => (xy, xy)) xys)
      | svgpathsize (ClosePath, xyxy) = xyxy
    (* Find the bounding box of SVG data *)
    fun svgsize (maxcharwidth, maxcharheight) =
      let
        val subheight = maxcharheight div 4
		    fun svgsize' (Svg []) = ((0, 0), (0, 0))
		      | svgsize' (Svg (svg :: svgs))
		      = foldl maxpair (svgsize' svg) (map svgsize' svgs)
		      | svgsize' (Text {x, y, text, anchor, ...})
		      =(case (String.size text * maxcharwidth, anchor) of
		          (width, "middle") =>
		          ((x - (width + 1) div 2, y - maxcharheight),
		           (x + (width + 1) div 2, y + subheight))
		        | (width, "end") =>
		          ((x - width, y - maxcharheight), (x, y + subheight))
		        | (width, _) =>
		          ((x, y - maxcharheight), (x + width, y + subheight)))
		      | svgsize' (Rectangle {x, y, width, height, ...})
		      = ((x, y), (x + width, y + height))
		      | svgsize' (Circle {cx, cy, r, ...})
		      = ((cx - r, cy - r), (cx + r, cy + r))
		      | svgsize' (Ellipse {cx, cy, rx, ry, ...})
		      = ((cx - rx, cy - ry), (cx + rx, cy + ry))
		      | svgsize' (Path {d, ...})
		      = foldl svgpathsize ((0, 0), (0, 0)) d
		  in
		    svgsize'
		  end
  end
    
  fun pathstr (MoveTo ((x, y) :: xys), s)
    = "\n    M" ^ intToString x ^ "," ^ intToString y ^
      pathstr (MoveTo xys, s)
    | pathstr (MoveTo [], s) = s
    | pathstr (LineTo ((x, y) :: xys), s)
    = "\n    L" ^ intToString x ^ "," ^ intToString y ^
      pathstr (LineTo xys, s)
    | pathstr (LineTo [], s) = s
    | pathstr (HLineTo (x :: xs), s)
    = "\n    H" ^ intToString x ^ pathstr (HLineTo xs, s)
    | pathstr (HLineTo [], s) = s
    | pathstr (VLineTo (y :: ys), s)
    = "\n    V" ^ intToString y ^ pathstr (VLineTo ys, s)
    | pathstr (VLineTo [], s) = s
    | pathstr (CurveTo (((x1, y1), (x2, y2), (x, y)) :: cs), s)
    = "\n    C" ^ intToString x1 ^ "," ^ intToString y1 ^
      " " ^ intToString x2 ^ "," ^ intToString y2 ^
      " " ^ intToString x  ^ "," ^ intToString y ^
      pathstr (CurveTo cs, s)
    | pathstr (CurveTo [], s) = s
    | pathstr (SCurveTo (((x2, y2), (x, y)) :: cs), s)
    = "\n    S" ^ intToString x2 ^ "," ^ intToString y2 ^
      " " ^ intToString x  ^ "," ^ intToString y ^
      pathstr (SCurveTo cs, s)
    | pathstr (SCurveTo [], s) = s
    | pathstr (QCurveTo (((x1, y1), (x, y)) :: cs), s)
    = "\n    Q" ^ intToString x1 ^ "," ^ intToString y1 ^
      " " ^ intToString x  ^ "," ^ intToString y ^
      pathstr (QCurveTo cs, s)
    | pathstr (QCurveTo [], s) = s
    | pathstr (SQCurveTo ((x, y) :: xys), s)
    = "\n    T" ^ intToString x  ^ "," ^ intToString y ^
      pathstr (SQCurveTo xys, s)
    | pathstr (SQCurveTo [], s) = s
    | pathstr (ClosePath, s) = "z" ^ s
  
  fun svgToString atts ns maxcharsize = 
    let
  fun str' (svg as (Svg svgs)) s
    = let
        val ((x1, y1), (x2, y2))
          = maxpair (((0, 0), (0, 0)), svgsize maxcharsize svg)
      in
        "<" ^ ns ^ "svg " ^ atts ^ "\n  width='" ^ intToString (x2 - x1 + 1) ^ 
        "' height='" ^ intToString (y2 - y1 + 1) ^ "' version='1.1'\n\
          \  fill='none' stroke='black'>\n" ^
          foldr (fn (svg, s) => str' svg s) ("</" ^ ns ^ "svg>\n" ^ s) svgs
      end
    | str' (Text {class, x, y, text, anchor}) s
    = "  <" ^ ns ^ "text " ^ (if class = "" then "" else "class='" ^ class ^ "' ") ^
      "x='" ^ intToString x ^ "' y='" ^ intToString y ^ "'" ^
      (if anchor = "" then "" else " text-anchor='" ^ anchor ^ "'") ^
      ">" ^ text ^ "</" ^ ns ^ "text>\n" ^ s
    | str' (Rectangle {class, x, y, r, width, height}) s
    = "  <" ^ ns ^ "rect " ^ (if class = "" then "" else "class='" ^ class ^ "' ") ^
      "x='" ^ intToString x ^ "' y='" ^ intToString y ^
      (if r = 0 then 
         ""
       else
         "' rx='" ^ intToString r ^ "' ry='" ^ intToString r) ^
      "' width='" ^ intToString width ^
      "' height='" ^ intToString height ^ "' />\n" ^ s
    | str' (Circle {class, cx, cy, r}) s
    = "  <" ^ ns ^ "circle " ^ (if class = "" then "" else "class='" ^ class ^ "' ") ^
      "cx='" ^ intToString cx ^ "' cy='" ^ intToString cy ^
      "' r='" ^ intToString r ^ "' />\n" ^ s
    | str' (Ellipse {class, cx, cy, rx, ry}) s
    = "  <" ^ ns ^ "ellipse " ^ (if class = "" then "" else "class='" ^ class ^ "' ") ^
      "cx='" ^ intToString cx ^ "' cy='" ^ intToString cy ^
      "' rx='" ^ intToString rx ^
      "' ry='" ^ intToString ry ^ "' />\n" ^ s
    | str' (Path {class, d}) s
    = "  <" ^ ns ^ "path " ^ (if class = "" then "" else "class='" ^ class ^ "' ") ^
      "d='" ^ foldr pathstr ("' />\n" ^ s) d
     in
       str'
     end

  exception MoveToExpected of pathdata list
  fun svgToTikZ unitsize =
  let
    val escapeLaTeX =
      String.translate
        (fn #"_" => "\\_" | #"$" => "\\$" | #"&" => "\\&"
          | #"{" => "\\{" | #"}" => "\\}" | c => String.str c)
    fun xy2str (x, y) = intToString x ^ "," ^ intToString y
    fun pair xy = "(" ^ xy2str xy ^ ")"
    fun styleis "" = "" | styleis class = "style=" ^ class
    fun pathToTikZ (MoveTo (xy0 :: xys) :: ds, s) =
      let
        infix 6 --
        fun (x1, y1) -- (x2, y2) = (x1 - x2, y1 - y2)
		    fun toTikZ _ _ ([], s) = s
		      | toTikZ c here (MoveTo [] :: ds, s)
		      = toTikZ c here (ds, s)
		      | toTikZ _ here (MoveTo (xy :: xys) :: ds, s)
		      = " " ^ pair xy ^ toTikZ here xy (MoveTo xys :: ds, s)
		      | toTikZ c here (LineTo [] :: ds, s)
		      = toTikZ c here (ds, s)
		      | toTikZ _ here (LineTo (xy :: xys) :: ds, s)
		      = " -- " ^ pair xy ^ toTikZ here xy (LineTo xys :: ds, s)
		      | toTikZ c here (HLineTo [] :: ds, s)
		      = toTikZ c here (ds, s)
		      | toTikZ _ (x0, y0) (HLineTo (x :: xs) :: ds, s)
		      = " -- (" ^ xy2str (x0, y0) ^ " -| " ^ xy2str (x, 0) ^ ")" ^
		        toTikZ (x0, y0) (x, y0) (HLineTo xs :: ds, s)
		      | toTikZ c here (VLineTo [] :: ds, s)
		      = toTikZ c here (ds, s)
		      | toTikZ _ (x0, y0) (VLineTo (y :: ys) :: ds, s)
		      = " -- (" ^ xy2str (x0, y0) ^ " |- " ^ xy2str (0, y) ^ ")" ^
		        toTikZ (x0, y0) (x0, y) (VLineTo ys :: ds, s)
		      | toTikZ c here (CurveTo [] :: ds, s)
		      = toTikZ c here (ds, s)
		      | toTikZ _ here
		          (CurveTo ((c1, c2, c) :: cs) :: ds, s)
		      = " .. controls +" ^ pair (c1 -- here) ^ " and +" ^
		        pair (c2 -- c) ^ " .. " ^
		        pair c ^ toTikZ c2 c (CurveTo cs :: ds, s)
		      | toTikZ c here (SCurveTo [] :: ds, s)
		      = toTikZ c here (ds, s)
		      | toTikZ (xc, yc) (x0, y0)
		          (SCurveTo ((c2, c) :: cs) :: ds, s)
		      = " .. controls +" ^ pair (x0 - xc, y0 - yc) ^
		        " and +" ^ pair (c2 -- c) ^ " .. " ^ pair c ^
		        toTikZ c2 c (SCurveTo cs :: ds, s)
		      | toTikZ c here (QCurveTo [] :: ds, s)
		      = toTikZ c here (ds, s)
		      | toTikZ _ here
		          (QCurveTo ((c1, c) :: cs) :: ds, s)
		      = " .. controls +" ^ pair (c1 -- here) ^ " .. " ^ pair c  ^
		        toTikZ c1 c (QCurveTo cs :: ds, s)
		      | toTikZ c here (SQCurveTo [] :: ds, s)
		      = toTikZ c here (ds, s)
		      | toTikZ (xc, yc) (x0, y0)
		          (SQCurveTo (c :: cs) :: ds, s)
		      = " .. controls +" ^ pair (x0 - xc, y0 - yc) ^
		        " .. " ^ pair c ^
		        toTikZ
		          (2 * x0 - xc, 2 * y0 - yc) c (SQCurveTo cs :: ds, s)
		      | toTikZ _ here (ClosePath :: ds, s)
		      = " -- " ^ pair xy0 ^ toTikZ here xy0 (ds, s)
      in
        " " ^ pair xy0 ^ toTikZ xy0 xy0 (MoveTo xys :: ds, s)
      end
      | pathToTikZ (ds, _) = raise MoveToExpected ds
    fun svgToTikZ' (Svg svgs) s
      = "\\tikzstyle nametext=[font=\\footnotesize\\itshape,inner sep=0pt]\n\
        \\\tikzstyle root=[dashed,rounded corners]\n\
        \\\tikzstyle binder=[draw,fill=white]\n\
        \\\tikzstyle node=[draw]\n\
        \\\tikzstyle nodetext=[font=\\sffamily\\bfseries,text=blue,inner sep=0pt]\n\
        \\\tikzstyle site=[fill=gray!25,rounded corners]\n\
        \\\tikzstyle sitetext=[font=\\sffamily,inner sep=0pt]\n\
        \\\tikzstyle link=[draw]\n\
        \\\begin{tikzpicture}[x={(" ^ Real.toString unitsize ^
        "cm,0cm)},y=-" ^ Real.toString unitsize ^ "cm]\n" ^
        foldr (fn (svg, s) => svgToTikZ' svg s) 
        ("\\end{tikzpicture}\n" ^ s)
        svgs
      | svgToTikZ' (Text {class, x, y, text, anchor}) s
      = "  \\draw " ^ pair (x, y) ^ " node [" ^ styleis class ^
        ",anchor=" ^ 
        (case anchor of
           "middle" => "south"
         | "end" => "south east"
         | _ => "south west") ^
        "] {" ^ escapeLaTeX text ^ "};\n" ^ s
      | svgToTikZ' (Rectangle {class, x, y, r, width, height}) s
      = "  \\draw[" ^ styleis class ^ "] " ^ pair (x, y) ^
        " rectangle +" ^ pair (width, height) ^ ";\n" ^ s
      | svgToTikZ' (Circle {class, cx, cy, r}) s
      = "  \\draw[" ^ styleis class ^ "] " ^ pair (cx, cy) ^
        " circle (" ^ Real.toString (real r * unitsize) ^ "cm);\n" ^
        s
      | svgToTikZ' (Ellipse {class, cx, cy, rx, ry}) s
      = "  \\draw[" ^ styleis class ^ "] " ^ pair (cx, cy) ^
        " ellipse (" ^ Real.toString (real rx * unitsize) ^ "cm and "
         ^ Real.toString (real ry * unitsize) ^ "cm);\n" ^ s
      | svgToTikZ' (Path {class, d}) s
      = "  \\draw" ^ pathToTikZ (d, ";\n" ^ s)
  in
    svgToTikZ'
  end

  (* SVG data types ************************************************)

  exception Impossible
  fun makeconfig f = f
  fun unmkconfig f = f
  fun defaultconfig (ctrlname, _) = {
      xsep           = 4,
      ysep           = 4,
      ctrlfontheight = 16,
      ctrlcharwidth  = 10,
      namefontheight = 10,
      namecharwidth  = 7,
      sitefontheight = 12,
      sitecharwidth  = 8,
      textmargin     = 2,
      textysep       = 3,
      idleedgelength = 8,
      rootwidth      = 40,
      rootheight     = 30,
      rootrounding   = 5,
      nodeminwidth   = Int.max (50, 10 * size ctrlname),
      nodeminheight  = 40,
      labelpos       = SW,
      portsep        = 7,
      binderradius   = 3,
      sitewidth      = 40,
      siteheight     = 30,
      siterounding   = 5
  }
 
  fun makesvg config b =
    let
      val config = getOpt (config, defaultconfig)
      val sqrt2        = Math.sqrt 2.0
      (* Coordinates for a regular octagon of width 1 *)
      val sqrt2div2    = sqrt2 / 2.0
      
      val showsitenums = Interface.width (BgBDNF.innerface b) > 1
      
      fun concatpairs [] = (NameMap.empty, [])
        | concatpairs [(m, svgs)] = (m, svgs)
        | concatpairs ((m, svgs) :: xs) = 
          let
            val (m', svgs') = concatpairs xs
          in
            (NameMap.plus (m, m'), svgs @ svgs')
          end
      (* Layout element e at (x, y), adding its svg to
       * the given list of svgs; return the next branch number,
       * the new x position (to the right of e plus x separator space)
       * and height of e if higher than maxheight.
       *) 
      fun hlayout xsep pp path
        (e, (branch, width, maxheight, (nws, sws, nes, ses), i, mksvgs)) =
        let
          val ((width', height), (nws', sws', nes', ses'), i, mksvg')
            = pp (branch :: path) i e
          fun xshift yadjust =
            let
              val yadjust = yadjust height
            in
              fn ((x, y), xys) => (x + width, yadjust y) :: xys
            end
        in
          (branch + 1,
           width + xsep + width',
           Int.max (height, maxheight),
           (fn yadjust => foldr (xshift yadjust) (nws yadjust) nws',
            fn yadjust => foldr (xshift yadjust) (sws yadjust) sws',
            fn yadjust => foldr (xshift yadjust) (nes yadjust) nes',
            fn yadjust => foldr (xshift yadjust) (ses yadjust) ses'),
           i,
           fn yadjust => fn xy => fn mknext =>
             mksvgs
               yadjust
               xy
               (fn (x, y) =>
                mksvg' (x, yadjust height y) ::
                  mknext (x + width' + xsep, y)))
        end
      open BgBDNF

      (* In the following, each syntactic rule of the BDNF is handled.
       * For each rule, the corresponding function takes
       * (- configuration data for the current location in the tree,)
       *  - a path, indicating the current location in the tree,
       *  - a permutation mapping in-order site numbers to correct
       *    inner site numbers (as given by the permutation in D),
       *  - a next in-order site number,
       *  - the current piece of syntax.
       * It then returns (width, height), (nws,sws,nes,ses), i and draw,
       * where
       *  - (width, height) is the space taken up by this node,
       *  - nws i a list of extreme coordinates on the north-western
       *    edge of this node, relative to the top-left position;
       *  - sws, nes, ses are for south-west, north-east and south-east
       *  - i is the next in-order site number,
       *  - draw is a function for actually drawing the node.
       * The draw function takes a coordinate (x,y) of the top left
       * position of the node (x grows rightwards, y grows downwards),
       * and returns
       *  - a point map, mapping each link (name or edge) to the 
       *    coordinate (x',y') of the point that links to it,
       *  - a list of SVG syntax representing the figure of the node
       * 
       * Finally, links are drawn in function ppB using the point map
       * and a list of links sxxs = [(s, xmin, xmax), ...] where s is the
       * substitution representing links, and the interval [xmin..xmax]
       * is the interval in which the links which are bound must be
       * drawn.
       *
       * The outer names are placed by attempting for each name to
       * place it at the average x position of all its points, adjusting
       * it if necessary to make room for other names.
       *
       * Single-point edges are drawn with a short vertical line at
       * their point.  A multiple-point edge is drawn at the average
       * x position of its points, above all its points, and heigher
       * the wider it must span.
       *
       * Node controls are aligned with the left edge of its node,
       * as low down as possible. 
       *)

      (* M: Molecules **********************************************)

      fun ppM path pi i m =
        let
          val {id_Z, KyX, N} = unmkM m
          val {ctrl, bound, free} = Ion.unmk KyX
          val (K, _, b, f) = Control.unmk ctrl
          val cfg as {
            ctrlfontheight, ctrlcharwidth, textysep, textmargin,
            nodeminwidth, nodeminheight, labelpos, portsep, binderradius,
            xsep, ysep, ...}
            = config (K, path)
          val textwidth = String.size K * ctrlcharwidth
          (* Calculate recursively children sizes *)
          val ((nwidth, nheight), (nws, sws, nes, ses), i, mksvgs)
            = ppN cfg path pi i N
          (* Calculate node ellipse size *)
          val (rwidth, rheight) = (real nwidth, real nheight)
          val normpts =
            foldr
              (fn (pts, pts') =>
               foldr
                 (fn ((x, y), pts') =>
                  (real x / rwidth, real y / rheight) :: pts')
                 pts'
                 pts)
              []
              [nws, sws, nes, ses]
          val ((rcx, rcy), r) =
            case normpts of
              [] => ((0.0,0.0), 0.0)
            | _  => Geometry2D.minenclosingcircle normpts
          val (cx, cy) = (round (rcx * rwidth), round (rcy * rheight))
          val width =
            Real.max
              (2.0 * ((r * real nwidth) + real xsep), real nodeminwidth)
          val height =
            Real.max
              (2.0 * ((r * real nheight) + real ysep), real nodeminheight) 
          val iwidth = round width
          val iheight = round height
          val hw = width / 2.0
          val hh = height / 2.0
          val halfwidth  = round hw
          val halfheight = round hh
          (* Calculate control text position *)
          val (above, left) =
            case labelpos of
              NW => (true, true)
            | NE => (true, false)
            | SE => (false, false)
            | _ => (false, true)
          val texty = (* y pos of control text relative to node ellipse top *)
            if textwidth >= halfwidth then
              0
            else
              let
                val dx = 1.0 - (real textwidth * 2.0 / width)
              in
                round (height * (1.0 - Math.sqrt (1.0 - dx * dx)) / 2.0)
              end
          val texty = if above then texty else iheight - texty + ctrlfontheight
          val yoff (* height of control text that lies below node ellipse top level *)
            = Int.min (texty, ctrlfontheight + textysep)
          val nodeyoff (* y position of node ellipse *)
            = ctrlfontheight + textysep - yoff
          val textypos = (* y position of text relative to boundingbox top *)
            Int.max (texty, ctrlfontheight)
          val textxpos = if left then 0 else iwidth - textwidth
          (* Define port drawing function *)
          fun ports (x0, y0) pmap svgs =
            let
              val space = iwidth - textwidth - textmargin
              val bspace = b * portsep
              val fspace = f * portsep
              val minspace = bspace + fspace
              val (start, myportsep) =
                if minspace > iwidth then (* too little room in general *)
                  (0, iwidth div b + f)
                else
                  case labelpos of
                    NW =>
                    if bspace >= textwidth then
                      (portsep div 2, portsep)
                    else (* adjust so free ports start after text *)
                      if space < fspace then
                        (0, portsep)
                      else
                        (textwidth + textmargin - b * portsep, portsep)
                  | _ => ((iwidth - minspace) div 2, portsep)
              fun placeport addmap addsvgs (n, (x, pmap, svgs)) =
                let
                  val x' = real x - hw (* relative to node centre *)
                  val normx = x' / hw  (* x' normalised to unit circle *)
                  val tmp = 1.0 - normx * normx
                  val y' = hh * (if tmp < 0.0 then 1.0 else Math.sqrt tmp)
                                       (* relative to node centre *)
                  val y'' = y0 + round (hh - y') (* absolute coor *)
                  val x'' = x0 + x               (* absolute coor *)
                  val svgs = addsvgs (n, (x'', y''), svgs)
                in
                  (x + myportsep,
                   addmap (n, (x'', y''), pmap),
                   svgs)
                end
              fun addboundlinks (X, (x, y), svgs) =
                let
                  fun doname n svgs =
                    case NameMap.lookup pmap n of
                      SOME (x', y') =>
                        Path {
                          class = "link",
                          d = [
                            MoveTo [(x, y + binderradius)],
                            CurveTo [
                              ((x, y + 10),
                               (x', y' - ctrlfontheight - textysep),
                               (x', y'))]]} :: svgs
                    | NONE => svgs
                  val svgs = 
                    Circle {
                      class = "binder", cx = x, cy = y, r = binderradius} ::
                    svgs
                in
                  NameSet.fold doname svgs X
                end
              val (x, pmap, bsvgs) =
                foldl 
                  (placeport (fn (_, _, pmap) => pmap) addboundlinks)
                  (start + myportsep div 2, NameMap.empty, [])
                  bound
              val (x, pmap, fsvgs) =
                foldl
                  (placeport NameMap.add (fn (_, _, svgs) => svgs))
                  (x, pmap, [])
                  free
            in
              (pmap, List.revAppend (bsvgs, List.revAppend (fsvgs, svgs)))
            end
          (* Calculate coordinates for enclosing octagon *)
          val xe = round (sqrt2div2 * width)
          val xw = iwidth - xe
          val ys = round (sqrt2div2 * height)
          val yn = iheight - ys
          (* Define drawing function *)
          fun draw (x, y) =
            let
              val (pmap1, svgs) = mksvgs (
               x + halfwidth - cx,
               y + halfheight - cy + ctrlfontheight - yoff)
              val (pmap2, svgs) = ports (x, y + ctrlfontheight + textysep - yoff) pmap1 svgs
            in
              (NameMap.plus (pmap1, pmap2),
              Ellipse {
                class = "node",
                cx = x + halfwidth,
                cy = y + halfheight + ctrlfontheight + textysep - yoff,
                rx = halfwidth, ry = halfheight} ::
              Text {
                class = "nodetext",
                x = x + textxpos,
                y = y + textypos,
                text = K, anchor = ""} :: 
              svgs)
            end
          val nws =
            case labelpos of
              NW => [
                (0, textypos - ctrlfontheight - textysep), (* top left corner of node text *)
                (xw, nodeyoff)]                      (* nnw corner of octagon *)
            | NE => [
                (yn, nodeyoff),                      (* wnw corner of octagon *)
                (0, textypos - ctrlfontheight - textysep), (* top left corner of node text *)
                (xw, nodeyoff)]                      (* nnw corner of octagon *)
            | _ => [
                (yn, nodeyoff),                      (* wnw corner of octagon *)
                (xw, nodeyoff)]                      (* nnw corner of octagon *)
          val nes =
            case labelpos of
              NW => [
                (textwidth, textypos - ctrlfontheight - textysep), (* top right corner of node text *)
                (xe, nodeyoff),                      (* nne corner of octagon *)
                (iwidth, yn + nodeyoff)]             (* ene corner of octagon *)
            | NE => [
                (iwidth, textypos - ctrlfontheight - textysep), (* top right corner of node text *)
                (xe, nodeyoff)]                      (* nne corner of octagon *)
            | _ => [
                (xe, nodeyoff),                      (* nne corner of octagon *)
                (iwidth, yn + nodeyoff)]             (* ene corner of octagon *)
          val sws =
            case (above, left) of
              (false, true) => [
                (0, textypos),                       (* bottom left corner of node text *)
                (xw, iheight + nodeyoff)]            (* ssw corner of octagon *)
            | (false, false) => [
                (0, ys + nodeyoff),                  (* wsw corner of octagon *)
                (xw, iheight + nodeyoff),            (* ssw corner of octagon *)
                (textxpos, textypos)]                (* bottom left corner of node text *)
            | _ => [
                (0, ys + nodeyoff),                  (* wsw corner of octagon *)
                (xw, iheight + nodeyoff)]            (* ssw corner of octagon *)
          val ses =
            case (above, left) of
              (false, true) => [
                (textwidth, textypos),               (* bottom right corner of node text *)
                (xe, iheight + nodeyoff),            (* sse corner of octagon *)
                (iwidth, ys + nodeyoff)]             (* ese corner of octagon *)
            | (false, false) => [
                (iwidth, textypos),                  (* bottom right corner of node text *)
                (xe, iheight + nodeyoff),            (* sse corner of octagon *)
                (iwidth, ys + nodeyoff)]             (* ese corner of octagon *)
            | _ => [
                (xe, iheight + nodeyoff),            (* sse corner of octagon *)
                (iwidth, ys + nodeyoff)]             (* ese corner of octagon *)
        in
          ((iwidth, Int.max (iheight + nodeyoff, textypos)),
           (nws, sws, nes, ses),
           i,
           draw)
        end

      (* S: Singular top-level nodes *******************************)

      and ppS pi path i s =
        case unmkS s of
          SCon (_, alpha) =>
          let
            val siteno = Int.toString (pi i)
            val {
             namecharwidth, sitewidth, siteheight, siterounding,
             sitefontheight, textysep, namefontheight, xsep, ...}
             = config (siteno, path)
            val ls = Wiring.unmk alpha
            val w =
              LinkSet.fold
                (fn l => fn w =>
                 w + 1 +
                 String.size 
                   (Name.ekam
                     (NameSet.someElement (#inner (Link.unmk l)))))
                0
                ls
            val nameswidth = 
              if w = 0 then 0 else (w - 1) * namecharwidth
            val mywidth = Int.max (sitewidth, nameswidth + 2 * xsep)
            val labelheight =
              if showsitenums then sitefontheight + textysep else 0
            fun draw (x, y) =
              let
                fun dolink l (x, pmap, svgs) =
                  let
                    val {outer, inner} = Link.unmk l
                    val oname = valOf outer
                    val iname = Name.ekam (NameSet.someElement inner)
                    val inamesize = String.size iname
                    val xpos = x + (inamesize * namecharwidth) div 2
                  in
                    (x + (inamesize + 1) * namecharwidth,
                     NameMap.add
                       (oname, (xpos, y + labelheight), pmap),
                     Text {
                       class = "nametext",
                       x = xpos, 
                       y = y + (namefontheight + labelheight + textysep),
                       text = iname,
                       anchor = "middle"} :: svgs)
                  end
                val (_, pmap, svgs) =
                  LinkSet.fold
                    dolink
                    (x + (mywidth - nameswidth) div 2, NameMap.empty, [])
                    ls
              in
                (pmap,
                 Rectangle {
                   class = "site",
                   x = x, y = y + labelheight, r = siterounding,
                   width = mywidth, height = siteheight} ::
                 (if showsitenums then
                    Text {
                      class = "sitetext",
                      x = x, y = y + sitefontheight,
                      text = siteno, anchor = ""} :: svgs
                  else
                    svgs))
              end
            val myheight = siteheight + labelheight
          in
            ((mywidth, myheight),
             ([(0, 0)], [(0, myheight)],
              [(mywidth, 0)], [(mywidth, myheight)]),
             i + 1,
             draw)
          end
        | SMol m => ppM path pi i m

      (* N: Name-discrete primes ***********************************)

      and ppN cfg path pi i n =
        let
          val {xsep, ...} = cfg
          val {absnames, G} = unmkN n
          val {idxmerge, Ss} = unmkG G
          val (_, width, maxheight, (nws, sws, nes, ses), i, mksvgs) =
            foldl
              (hlayout xsep (ppS pi) path)
              (0, 0, 0, 
               (fn _ => [], fn _ => [], fn _ => [], fn _ => []), i,
               fn _ => fn xy => fn mksvg => mksvg xy)
              Ss
          val xsep = case Ss of [] => 0 | _ => xsep
          fun yadjust height =
            let
              val yoff = (maxheight - height) div 2
            in
              fn y => y + yoff
            end
          val mksvgs = fn xy => mksvgs yadjust xy (fn _ => [])
          val corners = (nws yadjust, sws yadjust, nes yadjust, ses yadjust)
        in
          ((width - xsep, maxheight), corners, i, concatpairs o mksvgs)
        end

      (* P: Discrete primes ****************************************)

      fun ppP topsep pi path i p =
        let
          val cfg as {
            namecharwidth, rootwidth, rootheight, rootrounding,
            ctrlfontheight, textysep, xsep, ysep, ...} = config ("", path)
          val {id_Z, Y, s, X, N} = unmkP p
          val ls = Wiring.unmk s
          val w =
            LinkSet.fold
              (fn l => fn w =>
                 String.size (Name.ekam (valOf (#outer (Link.unmk l)))) + 1 + w)
              0
              ls
          val namewidth = if w = 0 then 0 else (w - 1) * namecharwidth
          val ((width, height), _, i, mksvgs) = ppN cfg path pi i N
          val mywidth' = width + 2 * xsep
          val mywidth =
            Int.max (mywidth', Int.max (namewidth, rootwidth))
          val myheight =
            Int.max (height + 2 * ysep + topsep, rootheight)
          fun draw (x, y) =
            let
              val (pmap, svgs) =
                mksvgs (
                  x + (mywidth - width) div 2,
                  y + ysep + topsep)
            in
             (pmap, (s, x + rootrounding, x + mywidth - rootrounding),
             Rectangle {
               class = "root",
               x = x, y = y,
               r = rootrounding,
               width = mywidth,
               height = myheight} ::
             svgs)
           end
        in
          ((mywidth, myheight),
           ([(0, 0)], [(0, myheight)],
            [(mywidth, 0)], [(mywidth, myheight)]),
           i, draw)
        end

      (* D: Discrete bigraphs **************************************)

      fun ppD cfg topsep d =
        let
          val {
            namecharwidth, namefontheight, rootheight,
            xsep, ysep, ...} : configinfo = cfg
          val {ren, Ps, perm} = unmkD d
          val w =
            case BgVal.match BgVal.PWir ren of
              BgVal.MWir w => w
            | _ => raise Impossible
          val ls = Wiring.unmk w
          val pi =
            case BgVal.match BgVal.PPer perm of 
              BgVal.MPer per => Permutation.invmap per
            | _ => raise Impossible 
          val (_, width, maxheight, _, _, mksvgs) =
            foldl
              (hlayout xsep (ppP topsep pi) [])
              (0, 0, 0,
               (fn _ => [], fn _ => [], fn _ => [], fn _ => []),
               0, fn _ => fn xy => fn mksvg => mksvg xy)
              Ps
          val xsep = case Ps of [] => 0 | _ => xsep
          val wd =
            NameSet.fold
              (fn n => fn wd => wd + 1 + String.size (Name.ekam n))
              0
              (Wiring.innernames w)
          val inameswidth = if wd = 0 then 0 else (wd - 1) * namecharwidth
          val wd =
            NameSet.fold
              (fn n => fn wd => wd + 1 + String.size (Name.ekam n))
              0
              (Interface.names (BgBDNF.outerface d))
          val onameswidth = if wd = 0 then 0 else (wd - 1) * namecharwidth
          val myheight =
            Int.max (rootheight, maxheight + ysep) + namefontheight
          fun draw (x, y) =
            let
              fun dolink l (x, pmap, svgs) =
                let
                  val {outer, inner} = Link.unmk l
                  val iname = Name.ekam (NameSet.someElement inner)
                  val inamesize = String.size iname
                  val xpos = x + (inamesize * namecharwidth) div 2
                in
                  (x + (1 + inamesize) * namecharwidth,
                   NameMap.add
                     (valOf outer,
                      (xpos,
                       y + myheight - namefontheight), 
                      pmap),
                   Text {
                     class = "nametext",
                     x = xpos, y = y + myheight,
                     text = iname, anchor = "middle"} :: svgs)
                end
              val mksvgs = fn xy => mksvgs (fn _ => fn y => y) xy (fn _ => [])
              fun merge ((pmap, sxx, svgs), (pmap', sxxs, svgs')) =
                (NameMap.plus (pmap, pmap'), sxx :: sxxs, svgs @ svgs')
              val (pmap, sxxs, svgs) =
                (foldl merge (NameMap.empty, [], []) o mksvgs) (x, y)
              val (_, pmap, svgs) =
                LinkSet.fold dolink (x, pmap, svgs) ls
            in
              (pmap, sxxs, svgs)
            end              
          val mywidth =
            Int.max (Int.max (width - xsep, inameswidth), onameswidth)
        in
          ((mywidth, myheight), draw)
        end

      (* B: Bigraphs ***********************************************)

      fun ppB b = 
        let
          val cfg as {
            namefontheight, namecharwidth, ctrlfontheight, 
            textysep, binderradius, idleedgelength, rootrounding,
            ysep, ...} = config ("", [])
          val {wirxid, D} = unmkB b
        in
          case BgVal.match (BgVal.PTen [BgVal.PWir, BgVal.PVar]) wirxid of
            BgVal.MTen [BgVal.MWir w, _] =>
            let
              val ls = Wiring.unmk w
              val edgecount =
                LinkSet.fold
                  (fn l => case Link.unmk l of
                     {outer = NONE, inner} => 
                      (fn i =>
                       if NameSet.size inner > 1 then i + 1 else i)
                    | _ => fn i => i)
                  0
                  ls
              val topsep = (* spacing between node top and prime edge *)
                if edgecount > 0 then
                  (edgecount + 3) * ysep
                else
                  if LinkSet.isEmpty ls then 0 else 4 * ysep
              val ((mywidth, _), mksvgs) = ppD cfg topsep D
              fun draw (x, y) = 
                let
                  val unique = ref 0
                  val (pmap, sxxs, svgs) =
                    mksvgs (x, y + namefontheight + textysep + binderradius)
                  fun dolink bound xmin xmax l (pmap, links) =
                    let
                      val {outer, inner} = Link.unmk l
                      val outer =
                        case outer of
                          SOME n => Name.ekam n
                        | NONE => ""
                      fun doname n (pmap, count, lox, hix, loy, sumx, xys) =
                        case NameMap.lookup pmap n of
                          NONE => (pmap, count, lox, hix, loy, sumx, xys)
                        | SOME (x, y) =>
                          (valOf (NameMap.remove (n, pmap)),
                           count + 1,
                           Int.min (lox, x),
                           Int.max (hix, x),
                           Int.min (loy, y),
                           sumx + x,
                           (x, y) :: xys)
                      val (pmap, count, lox, hix, loy, sumx, xys) =
                        NameSet.fold doname (pmap, 0, 999999, ~99, 999999, 0, []) inner
                      val outersize = String.size outer
                      val link = 
                        (if count > 0 then sumx div count else xmin,
                         if count > 0 then hix - lox else 0,
                         (unique := !unique + 1; !unique),
                         outersize,
                         bound,
                         outer,
                         xmin,
                         xmax,
                         loy,
                         xys)
                    in
                      (pmap, LSet.insert link links)
                    end
              fun addprime ((s, xmin, xmax), pmaplinks) =
                LinkSet.fold (dolink true xmin xmax) pmaplinks (Wiring.unmk s)
              val pmaplinks =
                LinkSet.fold (dolink false x (x + mywidth)) (pmap, LSet.empty) ls
              val (pmap, links) = foldl addprime pmaplinks sxxs
              fun adjust space [] = []
                | adjust space [l as (x1, dx, u, s, b, name, xmin, xmax, ymin, xys)]
                = if space >= 0 then
                    [l]
                  else
                    let
                      val x1' = x1 - space
                      val maxpos = xmax - (s * namecharwidth) div 2
                    in
                      [(Int.min (x1', maxpos), dx, u, s, b, name, xmin, xmax, ymin, xys)]
                    end
                | adjust
                    space
                    ((l1 as (x1, dx1, u1, s1, b1, name1, xmin1, xmax1, ymin1, xys1)) :: 
                     (l2 as (x2, dx2, u2, s2, b2, name2, xmin2, xmax2, ymin2, xys2)) :: links)
                = if space >= 0 then
                    l1 :: l2 :: links
                  else
                    let
                      val sumwidth = ((s1 + s2 + 2) * namecharwidth) div 2
                      val space' = x2 + space - x1 - sumwidth
                    in
                      if space' >= 0 then
                        (Int.min (xmax1, x1 - space),
                         dx1, u1, s1, b1, name1, xmin1, xmax1, ymin1, xys1) :: l2 :: links
                      else
                        case adjust space' (l2 :: links) of
                          [] => raise Impossible
                        | links2 as ((x2, _, _, _, _, _, _, _, _, _) :: _) =>
                          ((Int.max (xmin1, Int.min (xmax1, x2 - sumwidth)),
                            dx1, u1, s1, b1, name1, xmin1, xmax1, ymin1, xys1) :: links2)
                    end
              fun place ((l as (x1, dx, u, s, b, "", xmin, xmax, ymin, xys)) :: links1) links2
                = place links1 (l :: links2) (* Don't change x position of edges *)
                | place ((l as (x1, dx, u, s, b, name, xmin, xmax, ymin, xys)) :: links) []
                = if x1 < xmin + (s * namecharwidth) div 2 then (* Name introduction *)
                    place
                      links
                      [(xmax - (s * namecharwidth) div 2,
                        dx, u, s, b, name, xmin, xmax, ymin, xys)]
                  else
                    place links [l]
                | place ((l1 as (x1, dx1, u1, s1, b1, name1, xmin1, xmax1, ymin1, xys1)) :: links1)
                        ((l2 as (x2, _, _, s2, _, _, _, _, _, _)) :: links2)
                = let
                    val sumwidth = ((s2 + s1 + 2) * namecharwidth) div 2
                    val minxpos = xmin1 + (s1 * namecharwidth) div 2
                    val (x1', space) =
                      if x1 < minxpos then (* Name introduction *)
                        (Int.min (x2 - sumwidth, xmax1), 0)
                      else
                        (x1, x2 - x1 - sumwidth)
                  in
                    if space >= 0 then
                      place
                        links1
                        ((x1', dx1, u1, s1, b1, name1, xmin1, xmax1, ymin1, xys1) ::
                         l2 :: links2)
                    else
                      case adjust space (l2 :: links2) of
                        [] => raise Impossible
                      | links2 as ((x2, _, _, _, _, _, _, _, _, _) :: _) =>
                        place
                          links1
                          ((Int.max (x2 - sumwidth, xmin1 + (s1 * namecharwidth) div 2),
                            dx1, u1, s1, b1, name1, xmin1, xmax1, ymin1, xys1) :: links2)
                  end
                | place [] links = links
              val links = place (LSet.list links) []
              fun separate (e as (_,_,_,_,_,"",_,_,_,_), (es, ls)) = (e :: es, ls)
                | separate (l as (_,_,_,_,_, _,_,_,_,_), (es, ls)) = (es, l :: ls)
              val (edges, links) = foldr separate ([], []) links
              val edges = (* Calculate estimate for y positions of edges *)
                map
                  (fn (x, dx, u, s, b, name, xmin, xmax, ymin, xys) =>
                   (x, dx, u, s, b, name, xmin, xmax, ymin, 
                   ymin - ctrlfontheight - textysep - (dx - 40) div 10,
                   xys))
                  edges
              val edges = (EdgeSet.list o EdgeSet.fromList) edges (* sort them *)
              val (_, edges) = (* Adjust so edges are spaced below prime top *)
                foldl
                  (fn (e as (x, dx, u, s, b, name, xmin, xmax, ymin, y, xys),
                       (lasty, es)) =>
                     let
                       val limit = lasty + ysep
                       val y = if y < limit then limit else y
                     in
                       (y, (x, dx, u, s, b, name, xmin, xmax, ymin, y, xys) :: es)
                     end)
                  (namefontheight + textysep + binderradius, [])
                  edges
              fun drawedges [] svgs = svgs
                | drawedges ((xx, dx, _, _, _, _, _, _, ymin, yy, xys) :: edges) svgs
                = let
                    fun drawedge' [] svgs = svgs
                      | drawedge' ((x', y') :: xys) svgs
                      = drawedge'
                          xys
                          (Path {
                            class = "edge",
                            d = [
                              MoveTo [(x', y')],
                              CurveTo [(
                                (x', y' - ctrlfontheight - textysep),
                                (x', yy), 
                                (xx, yy))]]} :: svgs)
                    fun drawedge [] svgs = svgs
                      | drawedge [(x', y')] svgs = 
                        (Path {
                           class = "edge",
                           d = [
                             MoveTo [(x', y')],
                             LineTo[(x', y' - idleedgelength)]]} :: svgs)
                      | drawedge xys svgs = drawedge' xys svgs
                  in
                    drawedge xys (drawedges edges svgs)
                  end
              fun drawlinks [] svgs = svgs
                | drawlinks ((xx, _, _, _, bound, name, _, _, _, xys) :: links) svgs
                = let
                    val namey =
                      if bound then
                        y + namefontheight + textysep + 2 * binderradius
                      else
                        y + namefontheight + textysep
                    fun drawlink [] svgs = svgs
                      | drawlink ((x', y') :: xys) svgs
                      = drawlink
                          xys
                          (Path {
                            class = "link",
                            d = [
                             MoveTo [(x', y')],
                             CurveTo [
                               ((x', y' - ctrlfontheight - textysep - 5),
                                (xx, namey + 20),
                                (xx, namey))]]}
                           :: svgs)
                    val svgs = Text {
                         class = "nametext", x = xx, y = y + namefontheight,
                         text = name, anchor = "middle"} ::
                      drawlinks links svgs
                  in
                    drawlink
                      xys
                      (if bound then
                         Circle {
                           class = "binder",
                           cx = xx, 
                           cy = y + namefontheight + textysep + binderradius,
                           r = binderradius} ::
                         svgs
                       else
                         svgs)
                  end
              val svgs = svgs @ drawlinks links (drawedges edges [])
                in
                  svgs
                end
            in
              draw
            end
          | m => raise Impossible
        end
    in
      Svg (ppB b (0, 0))
    end

  fun ppsvg atts ns config b =
    let
      val {ctrlcharwidth, ctrlfontheight, ...} =
        getOpt (config, defaultconfig) ("", [])
    in
      svgToString atts ns (ctrlcharwidth, ctrlfontheight) (makesvg config b) ""
    end

  fun ppsvgdoc config b =
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\"?>\n\
    \<?xml-stylesheet type=\"text/css\"\n\
    \  href=\"http://www.itu.dk/research/theory/bpl/css/bplsvg.css\" ?>\n\
    \<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 20010904//EN\"\n\
    \\"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd\">\n" ^
    ppsvg "xmlns='http://www.w3.org/2000/svg'" "" config b

  fun ppxhtmldoc title config b =
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\"?>\n\
    \<?xml-stylesheet type=\"text/css\"\n\
    \  href=\"http://www.itu.dk/research/theory/bpl/css/bplsvg.css\" ?>\n\
    \<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"\n\
  	\ \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n\
    \<html xmlns='http://www.w3.org/1999/xhtml'\n\
    \  xmlns:svg='http://www.w3.org/2000/svg'\n\
    \  xml:lang='en'>\n\
    \<head>\n\
    \  <title>" ^ title ^ "</title>\n\
    \  <object id='AdobeSVG' classid='clsid:78156a80-c6a1-4bbf-8e6a-3cd390eeb4e2'></object>\n\
    \  <?import namespace='svg' implementation='#AdobeSVG'?>\n\
    \</head>\n\
    \<body>\n  " ^
    ppsvg "" "svg:" config b ^
    "\n</body>\n\
    \</html>\n"
  fun ppTikZ unitsize config b =
    svgToTikZ (getOpt (unitsize, 0.02)) (makesvg config b) ""
end
