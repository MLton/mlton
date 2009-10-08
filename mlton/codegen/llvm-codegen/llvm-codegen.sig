(* Copyright (C) 2009 Wesley W. Terpstra.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LLVM_CODEGEN_STRUCTS =
   sig
      structure Ffi: FFI
      structure Rssa: RSSA
      sharing Ffi.CFunction = Rssa.CFunction
   end

signature LLVM_CODEGEN =
   sig
      include LLVM_CODEGEN_STRUCTS

      val implementsPrim: 'a Rssa.Prim.t -> bool
      val output: {program: Rssa.Program.t,
                   outputLL: unit -> {file: File.t,
                                      print: string -> unit,
                                      done: unit -> unit}
                   } -> unit
   end
