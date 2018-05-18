(* Copyright (C) 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ABS_REP =
   sig
      type t
      structure Rep : sig type t end
      val arrayFromRep : Rep.t array -> t array
      val arrayToRep : t array -> Rep.t array
      val fromRep : Rep.t -> t
      val listFromRep : Rep.t list -> t list
      val listToRep : t list -> Rep.t list
      val toRep : t -> Rep.t
      val vectorFromRep : Rep.t vector -> t vector
      val vectorToRep : t vector -> Rep.t vector
   end

signature ABS_REP_EQ =
   sig
      eqtype t
      structure Rep : sig eqtype t end
      val arrayFromRep : Rep.t array -> t array
      val arrayToRep : t array -> Rep.t array
      val fromRep : Rep.t -> t
      val listFromRep : Rep.t list -> t list
      val listToRep : t list -> Rep.t list
      val toRep : t -> Rep.t
      val vectorFromRep : Rep.t vector -> t vector
      val vectorToRep : t vector -> Rep.t vector
   end
