(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure PosixFlags: POSIX_FLAGS_EXTRA =
   struct
      type flags = word
	 
      fun toWord f = f
      fun wordTo f = f
	 
      val flags: flags list -> flags = List.foldl Word.orb 0w0
	 
      fun anySet(f, f') = Word.andb(f, f') <> 0w0

      fun allSet(f, f') = Word.andb(f, f') = f

      val empty: flags = 0w0
   end
