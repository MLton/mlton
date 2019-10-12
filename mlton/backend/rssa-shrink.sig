(* Copyright (C) 2009,2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature RSSA_SHRINK_STRUCTS =
   sig
      include RSSA_TYPE_CHECK
   end

signature RSSA_SHRINK =
   sig
      include RSSA_SHRINK_STRUCTS

      val shrinkFunction:
         {main: Function.t} -> {main: unit -> Function.t,
                                shrink: Function.t -> Function.t}
      val shrink: Program.t -> Program.t
   end
