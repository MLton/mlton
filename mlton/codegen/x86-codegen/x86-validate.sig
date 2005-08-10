(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
