(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
