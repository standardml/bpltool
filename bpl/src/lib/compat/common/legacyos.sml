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

(** OS structure matching the OS signature from
 * the new SML Basis Library, but based on the old
 * SML Basis Library OS structure.
 * @version $LastChangedRevision$
 *)

structure OS :> OS =
struct

  open OS
  
  structure FileSys : OS_FILE_SYS =
  struct
    open OS.FileSys
    val readDir
      = fn ds => case readDir ds of "" => NONE | s => SOME s
  end

    
end (* structure OS *)
