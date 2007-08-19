(* Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature BYTECODE_STRUCTS = 
   sig
      structure CCodegen: C_CODEGEN
      structure Machine: MACHINE
      sharing Machine = CCodegen.Machine
   end

signature BYTECODE = 
   sig
      include BYTECODE_STRUCTS

      val implementsPrim: 'a Machine.Prim.t -> bool
      val output: {program: Machine.Program.t,
                   outputC: unit -> {file: File.t,
                                     print: string -> unit,
                                     done: unit -> unit}} -> unit
   end
