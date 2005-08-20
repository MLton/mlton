(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PARSE =
   sig
      datatype result =
         Alias of File.t
       | Bad of string (* error message *)
       | Members of File.t list

      (* Pre: cmfile must not contain any path, i.e. it must be in the
       *      current directory.
       * The resulting members are either absolute or relative to the current
       * directory.
       *)
      val parse: {cmfile: string} -> result
   end
