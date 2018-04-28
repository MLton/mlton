(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

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
    
    val picRelative : unit -> (x86.Label.t -> x86.Label.t) * 
                              x86.Register.t option
  end
