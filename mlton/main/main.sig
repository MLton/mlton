(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature MAIN =
   sig
      val commandLine: string list -> OS.Process.status
      val exportMLton: unit -> unit
      val exportNJ: Dir.t * File.t -> unit
   end
