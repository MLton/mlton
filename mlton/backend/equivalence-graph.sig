(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature EQUIVALENCE_GRAPH_STRUCTS = 
   sig
   end

(* An equivalence graph is an equivalence relation with a weight function on
 * classes and an edge relation between classes.
 *
 * The main operation is coarsen, which takes an equivalence graph and coarsens
 * the equivalence relation so that the class weights are as large as possible
 * subject to a constraint.
 *)

signature EQUIVALENCE_GRAPH = 
   sig
      include EQUIVALENCE_GRAPH_STRUCTS

      structure Class:
         sig
            (* The type of equivalence classes. *)
            type t

            val plist: t -> PropertyList.t
         end

      (* The type of equivalence graphs. *)
      type t

      (* Make two classes equivalent.
       * The size of the resulting class is the sum of the sizes of the original
       * two classes.  This is a no-op if the classes are already equivalent.
       *)
      val == : t * Class.t * Class.t -> unit

      (* Add a new edge between two classes. *)
      val addEdge: t * Class.t * Class.t -> unit

      (* Make the equivalence relation as coarse as possible so that the
       * number of edges between classes is minimized, subject to the constraint
       * that the sum of the node sizes in an equivalence class is
       * <= maxClassSize.  Classes for which this constraint was violated by
       * previous calls to == should not be made coarser.
       *)
      val coarsen: t * {maxClassSize: int} -> unit

      (* Return a new relation. *)
      val new: unit -> t

      (* newClass (g, {classSize}) adds a new class to the equivalence graph. *)
      val newClass: t * {size: int} -> Class.t
   end
