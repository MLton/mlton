(* Copyright (C) 2009 Matthew Fluet.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature EXPORT =
   sig
      val exportMLton: unit -> unit
      val exportNJ: File.t -> unit
   end
