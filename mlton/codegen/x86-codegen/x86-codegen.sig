(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature X86_CODEGEN_STRUCTS =
  sig
    structure MachineOutput: MACHINE_OUTPUT
  end

signature X86_CODEGEN =
  sig
    include X86_CODEGEN_STRUCTS

    val output: {program: MachineOutput.Program.t,
                 includes: string list,
                 outputC: unit -> {file: File.t,
				   print: string -> unit,
				   done: unit -> unit},
                 outputS: unit -> {file: File.t,
				   print: string -> unit,
				   done: unit -> unit}}
                -> unit
  end

