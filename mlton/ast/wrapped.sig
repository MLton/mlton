(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
   
signature WRAPPED =
   sig
      type node'
      type obj

      val dest: obj -> node' * Region.t
      val make: node' -> obj
      val makeRegion': node' * SourcePos.t * SourcePos.t -> obj
      val makeRegion: node' * Region.t -> obj
      val node: obj -> node'
      val region: obj -> Region.t
   end
