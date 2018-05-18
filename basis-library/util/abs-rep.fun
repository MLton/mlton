(* Copyright (C) 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MkAbsRep(type rep) :> ABS_REP where type Rep.t = rep =
   struct
      structure Rep = struct type t = rep end
      type t = Rep.t
      val arrayFromRep : rep array -> t array = fn x => x
      val arrayToRep : t array -> rep array = fn x => x
      val fromRep : rep -> t = fn x => x
      val listFromRep : rep list -> t list = fn x => x
      val listToRep : t list -> rep list = fn x => x
      val toRep : t -> rep = fn x => x
      val vectorFromRep : rep vector -> t vector = fn x => x
      val vectorToRep : t vector -> rep vector = fn x => x
   end

functor MkAbsRepEq(eqtype rep) :> ABS_REP_EQ where type Rep.t = rep =
   struct
      structure Rep = struct type t = rep end
      type t = Rep.t
      val arrayFromRep : rep array -> t array = fn x => x
      val arrayToRep : t array -> rep array = fn x => x
      val fromRep : rep -> t = fn x => x
      val listFromRep : rep list -> t list = fn x => x
      val listToRep : t list -> rep list = fn x => x
      val toRep : t -> rep = fn x => x
      val vectorFromRep : rep vector -> t vector = fn x => x
      val vectorToRep : t vector -> rep vector = fn x => x
   end
