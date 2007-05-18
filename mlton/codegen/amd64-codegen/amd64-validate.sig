(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AMD64_VALIDATE_STRUCTS =
  sig
    structure amd64 : AMD64
  end

signature AMD64_VALIDATE =
  sig
    include AMD64_VALIDATE_STRUCTS

    val validate : {assembly: amd64.Assembly.t list list} -> bool

    val validate_totals : unit -> unit
  end
