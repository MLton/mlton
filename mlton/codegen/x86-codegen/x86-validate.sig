(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

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
