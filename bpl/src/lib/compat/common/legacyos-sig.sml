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

(** OS signature from the new SML Basis Library.
 * @version $LastChangedRevision$
 *)

signature OS_FILE_SYS =
sig
   type dirstream

    val openDir : string -> dirstream
    val readDir : dirstream -> string option
    val rewindDir : dirstream -> unit
    val closeDir : dirstream -> unit

    val chDir : string -> unit
    val getDir : unit -> string
    val mkDir : string -> unit
    val rmDir : string -> unit
    val isDir : string -> bool
    val isLink : string -> bool
    val readLink : string -> string
    val fullPath : string -> string
    val realPath : string -> string
    val modTime : string -> Time.time
    val fileSize : string -> Position.int
    val setTime : string * Time.time option -> unit
    val remove : string -> unit
    val rename : {old : string, new : string} -> unit

    datatype access_mode = A_READ | A_WRITE | A_EXEC

    val access : string * access_mode list -> bool

    val tmpName : unit -> string

    eqtype file_id

    val fileId : string -> file_id
    val hash : file_id -> word
    val compare : file_id * file_id -> order 

end

signature OS = sig
    structure FileSys : OS_FILE_SYS
    structure IO : OS_IO
    structure Path : OS_PATH
    structure Process : OS_PROCESS

    eqtype syserror

    exception SysErr of string * syserror option

    val errorMsg : syserror -> string
    val errorName : syserror -> string
    val syserror  : string -> syserror option 
 end