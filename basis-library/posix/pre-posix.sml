(* Copyright (C) 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PrePosix =
   struct
      structure FileDesc = MkAbsRepEq(type rep = C_Fd.t)
      structure GId = MkAbsRepEq(type rep = C_GId.t)
      structure PId = MkAbsRepEq(type rep = C_PId.t)
      structure Signal = MkAbsRepEq(type rep = C_Signal.t)
      structure SysError = MkAbsRepEq(type rep = C_Int.t)
      structure UId = MkAbsRepEq(type rep = C_UId.t)
   end
