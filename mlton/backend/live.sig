(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LIVE_STRUCTS = 
   sig
      include RSSA
   end

signature LIVE = 
   sig
      include LIVE_STRUCTS

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
