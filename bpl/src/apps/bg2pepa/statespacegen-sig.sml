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

(** Abstract data type for modelling and generating bigraph aspect state
 * spaces. Finds a fixed point for an approximation to the transition
 * relation, thereby generating at least the reachable aspect states.
 * FIXME elaborate and make more formal...
 *
 * @version $LastChangedRevision: 2717 $
 *)
signature STATESPACEGEN =
sig
  type absbg
  type pcrule
  type state
  type transitions

  val gen_state_space : {agent : absbg, rules : pcrule list}
                        -> {initial : state, transitions : transitions}
end
