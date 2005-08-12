(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature WRAPPED =
   sig
      type node'
      type obj

      val dest: obj -> node' * Region.t
      val makeRegion': node' * SourcePos.t * SourcePos.t -> obj
      val makeRegion: node' * Region.t -> obj
      val node: obj -> node'
      val region: obj -> Region.t
   end
