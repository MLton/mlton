(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor BitFlags(val all: SysWord.word): BIT_FLAGS_EXTRA =
   struct
      type flags = SysWord.word
         
      val all: flags = all
      val empty: flags = 0w0

      fun toWord f = f
      fun fromWord f = SysWord.andb(f, all)

      val flags: flags list -> flags = List.foldl SysWord.orb empty

      val intersect: flags list -> flags = List.foldl SysWord.andb all

      fun clear(f, f') = SysWord.andb(SysWord.notb f, f')

      fun allSet(f, f') = SysWord.andb(f, f') = f

      fun anySet(f, f') = SysWord.andb(f, f') <> 0w0

   end
structure BitFlags = BitFlags(val all = 0wxFFFF: SysWord.word)
