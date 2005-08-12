(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature X86_VALIDATE_STRUCTS =
  sig
    structure x86 : X86
  end

signature X86_VALIDATE =
  sig
    include X86_VALIDATE_STRUCTS

    val validate : {assembly: x86.Assembly.t list list} -> bool

    val validate_totals : unit -> unit
  end
