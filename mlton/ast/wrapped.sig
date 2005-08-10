(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
