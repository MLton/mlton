(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature CMM_CODEGEN_STRUCTS =
   sig
      structure CCodegen: C_CODEGEN
      structure Machine: MACHINE
      sharing Machine = CCodegen.Machine
   end

signature CMM_CODEGEN =
   sig
      include CMM_CODEGEN_STRUCTS

      val implementsPrim: 'a Machine.Prim.t -> bool
      val output: {program: Machine.Program.t,
		   outputC: unit -> {file: File.t,
				     print: string -> unit,
				     done: unit -> unit},
		   outputCmm: unit -> {file: File.t,
				       print: string -> unit,
				       done: unit -> unit}
		   } -> unit
   end
