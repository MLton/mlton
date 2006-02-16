(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature KNOWN_CASE_STRUCTS = 
  sig
    include RESTORE
  end

signature KNOWN_CASE =
  sig
    include KNOWN_CASE_STRUCTS

    val simplify: Program.t -> Program.t
  end