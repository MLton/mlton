(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t
type word = Word.t

signature X86_CODEGEN_STRUCTS =
   sig
      structure CCodegen: C_CODEGEN
      structure Machine: MACHINE
      sharing Machine = CCodegen.Machine
   end

signature X86_CODEGEN =
  sig
    include X86_CODEGEN_STRUCTS

    val output: {program: Machine.Program.t,
                 outputC: unit -> {file: File.t,
				   print: string -> unit,
				   done: unit -> unit},
		 outputH: unit -> {done: unit -> unit,
				   print: string -> unit},
                 outputS: unit -> {file: File.t,
				   print: string -> unit,
				   done: unit -> unit}}
                -> unit
  end

