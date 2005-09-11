(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature MLTON_ITIMER =
   sig
      datatype t =
         Prof
       | Real
       | Virtual

      val set: t * {interval: Time.time, value: Time.time} -> unit
      val signal: t -> Posix.Signal.signal
   end
