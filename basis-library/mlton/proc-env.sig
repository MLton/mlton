(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

signature MLTON_PROC_ENV =
   sig
      val setenv: {name: string, value: string} -> unit
      val setgroups: Posix.ProcEnv.gid list -> unit
   end
