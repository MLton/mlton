(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor BitFlags(structure S : sig
                    eqtype t
                    val castToSysWord: t -> SysWord.word
                    val castFromSysWord: SysWord.word -> t
                    val andb: t * t -> t
                    val notb: t -> t
                    val orb: t * t -> t
                 end): BIT_FLAGS_EXTRA =
   struct
      type flags = S.t

      val all: flags = S.castFromSysWord (SysWord.~ 0w1)
      val empty: flags = S.castFromSysWord 0w0

      fun toWord f = S.castToSysWord f
      fun fromWord w = S.castFromSysWord (SysWord.andb (w, toWord all))

      val flags: flags list -> flags = List.foldl S.orb empty

      val intersect: flags list -> flags = List.foldl S.andb all

      fun clear (f, f') = S.andb (S.notb f, f')

      fun allSet (f, f') = S.andb (f, f') = f'

      fun anySet (f, f') = S.andb (f, f') <> empty
   end
