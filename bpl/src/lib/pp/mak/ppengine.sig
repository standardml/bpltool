(*
 *   Copyright 2001 Henning Makholm
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

(*signature PPengine = PPengine*)
structure PPengine : PPengine
(*signature PPbuild = PPbuild where type 'a pptree' = 'a PPengine.pptree*)
structure PPbuild : PPbuild where type 'a pptree' = 'a PPengine.pptree
infixr 5 ^+
infix 4 +^
infixr 4 ++
