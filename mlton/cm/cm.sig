(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature CM =
   sig
      (* cmfile can be relative or absolute.
       * The resulting list of files will have the same path as cmfile.
       *)
      val cm: {cmfile: File.t} -> File.t list
   end
