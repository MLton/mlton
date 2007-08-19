(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonProcEnv: MLTON_PROC_ENV =
   struct
      type gid = C_GId.t

      fun setenv {name, value} =
         let
            val name = NullString.nullTerm name
            val value = NullString.nullTerm value
         in
            PosixError.SysCall.simple
            (fn () => PrimitiveFFI.Posix.ProcEnv.setenv (name, value))
         end

      fun setgroups gs =
         let
            val v = Vector.fromList gs
            val n = Vector.length v
         in
            PosixError.SysCall.simple
            (fn () => PrimitiveFFI.Posix.ProcEnv.setgroups (C_Int.fromInt n, v))
         end
   end
