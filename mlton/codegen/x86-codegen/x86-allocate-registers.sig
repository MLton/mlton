(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature X86_ALLOCATE_REGISTERS_STRUCTS =
  sig
    structure x86 : X86
    structure x86MLton : X86_MLTON
    sharing x86 = x86MLton.x86
  end

signature X86_ALLOCATE_REGISTERS =
  sig
    include X86_ALLOCATE_REGISTERS_STRUCTS

    val allocateRegisters : {assembly: x86.Assembly.t list list,
			     liveness: bool} -> 
                            x86.Assembly.t list list

    val allocateRegisters_totals : unit -> unit
  end
