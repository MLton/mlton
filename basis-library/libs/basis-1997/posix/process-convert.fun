(* Copyright (C) 2002-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor PosixProcessConvert
        (structure Process: POSIX_PROCESS) :
        POSIX_PROCESS_1997 =
  struct
     open Process
     structure W =
        struct
	   open W
	   structure Flags = FlagsConvert(structure Flags = W)
	   open Flags
	end
  end