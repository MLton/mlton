signature LLVM_CODEGEN_STRUCTS =
   sig
       structure Ffi: FFI
       structure Machine: MACHINE
   end

signature LLVM_CODEGEN =
   sig
       include LLVM_CODEGEN_STRUCTS

       val implementsPrim: 'a Machine.Prim.t -> bool
       val output: {program: Machine.Program.t,
                    outputLL: unit -> {file: File.t,
                                       print: string -> unit,
                                       done: unit -> unit}
                   } -> unit
   end

