(* Copyright (C) 2019 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RSSA_LIVE_STRUCTS =
   sig
      include RSSA_SHRINK
   end

signature RSSA_LIVE =
   sig
      include RSSA_LIVE_STRUCTS

      structure Live:
         sig
            val live:
               Function.t * {shouldConsider: Var.t -> bool}
               -> {labelLive:
                   Label.t -> {(* live at beginning of block. *)
                               begin: Var.t vector,
                               (* live at the beginning of a block, except formals. *)
                               beginNoFormals: Var.t vector,
                               (* live handler slots at beginning of block. *)
                               handler: Label.t option,
                               link: bool},
                   remLabelLive: Label.t -> unit}
         end
   end
