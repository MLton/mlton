(* Copyright (C) 2022 Matthew Fluet.
 * Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_PROCESS =
   sig
      type pid

      val spawn: {args: string list, path: string} -> pid
      val spawne: {args: string list, env: string list, path: string} -> pid
      val spawnp: {file: string, args: string list} -> pid
   end
