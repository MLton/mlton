(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
   
signature EQUIVALENCE_GRAPH_STRUCTS = 
   sig
   end

signature EQUIVALENCE_GRAPH = 
   sig
      include EQUIVALENCE_GRAPH_STRUCTS
      
      structure Class:
	 sig
	    (* The type of equivalence classes. *)
	    type t

	    val plist: t -> PropertyList.t
	 end

      (* The type of directed graphs with equivalence relations on nodes. *)
      type t

      (* Return a new graph. *)
      val new: unit -> t

      (* newNode(g, i) adds a new node to graph g, where the size of the node
       * is i.  The new node is not equivalent to any other node.  Return the
       * class of the node.
       *)
      val newClass: t * int -> Class.t

      (* Add a new edge between two classes.
       * Increment the weight of the edge if it's already there.
       *)
      val addEdge: t * {from: Class.t, to: Class.t} -> unit

      (* Make two classes equivalent.
       * The size of the resulting class is the sum of the sizes of the original
       * two classes.  This is a noop if the classes are already equal.
       *)
      val == : t * Class.t * Class.t -> unit

      (* Make the equivalence relation as coarse as possible so that the
       * number of edges between classes in minimized, subject to the constraint
       * that the sum of the node sizes in an equivalence class is <= maxNodeSize.
       * Classes for which this constraint was violated by previous calls to ==
       * should not be made coarser.
       *)
      val greedy: {graph: t, maxClassSize: int} -> unit
   end
