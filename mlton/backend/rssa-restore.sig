(* Copyright (C) 2019-2020 Jason Carr, Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RSSA_RESTORE_STRUCTS =
   sig
      include RSSA_LIVE
   end

signature RSSA_RESTORE =
   sig
      include RSSA_RESTORE_STRUCTS

      val restoreFunction:
         {main: Function.t, statics: {dst: Var.t * Type.t, obj: Object.t} vector}
         -> {main: Function.t, restore: Function.t -> Function.t}
      val restore: Program.t -> Program.t
   end
