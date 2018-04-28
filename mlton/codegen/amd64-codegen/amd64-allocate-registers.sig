(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AMD64_ALLOCATE_REGISTERS_STRUCTS =
  sig
    structure amd64 : AMD64
    structure amd64MLton : AMD64_MLTON
    sharing amd64 = amd64MLton.amd64
  end

signature AMD64_ALLOCATE_REGISTERS =
  sig
    include AMD64_ALLOCATE_REGISTERS_STRUCTS

    val allocateRegisters : {assembly: amd64.Assembly.t list list,
                             liveness: bool} -> 
                            amd64.Assembly.t list list

    val allocateRegisters_totals : unit -> unit
  end
