(* Copyright (C) 2013 Matthew Fluet, Brian Leibig.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LLVM_CODEGEN_STRUCTS =
   sig
       structure Ffi: FFI
       structure CCodegen: C_CODEGEN
       structure Machine: MACHINE
       sharing Machine = CCodegen.Machine
   end

signature LLVM_CODEGEN =
   sig
       include LLVM_CODEGEN_STRUCTS

       val implementsPrim: 'a Machine.Prim.t -> bool
       val output: {program: Machine.Program.t,
                    outputC: unit -> {file: File.t,
                                      print: string -> unit,
                                      done: unit -> unit},
                    outputLL: unit -> {file: File.t,
                                       print: string -> unit,
                                       done: unit -> unit}
                   } -> unit
   end
