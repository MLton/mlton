(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor BitFlags(structure S : sig
                    type t
                    val all: t
                    val toSysWord: t -> SysWord.word
                    val fromSysWord: SysWord.word -> t
                 end): BIT_FLAGS_EXTRA =
   struct
      type flags = S.t
         
      val all: flags = S.all
      val empty: flags = S.fromSysWord 0w0

      fun toWord f = W.toSysWord f
      fun fromWord w = W.fromSysWord (SysWord.andb(w, toWord all))

      val flags: flags list -> flags = List.foldl W.orb empty

      val intersect: flags list -> flags = List.foldl W.andb all

      fun clear(f, f') = W.andb(W.notb f, f')

      fun allSet(f, f') = W.andb(f, f') = f

      fun anySet(f, f') = W.andb(f, f') <> empty
   end
