(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature AMD64_CODEGEN_STRUCTS =
   sig
      structure CCodegen: C_CODEGEN
      structure Machine: MACHINE
      sharing Machine = CCodegen.Machine
   end

signature AMD64_CODEGEN =
   sig
      include AMD64_CODEGEN_STRUCTS

      val implementsPrim: Machine.Type.t Machine.Prim.t -> bool
      val output: {program: Machine.Program.t,
                   outputC: unit -> {file: File.t,
                                     print: string -> unit,
                                     done: unit -> unit},
                   outputS: unit -> {file: File.t,
                                     print: string -> unit,
                                     done: unit -> unit}} -> unit
   end
