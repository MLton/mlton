(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

      val classifyEdges: Graph.t -> {discover: Graph.Node.t -> int,
				      finish: Graph.Node.t -> int}
	 -> {tree: Graph.Edge.t list ref,
	     forward: Graph.Edge.t list ref,
	     back: Graph.Edge.t list ref,
	     cross: Graph.Edge.t list ref}
	 * Param.t
