(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
signature COMPILE =
   sig
      val compile: {input: File.t list,
		    outputC: unit -> {file: File.t,
				      print: string -> unit,
				      done: unit -> unit},
		    outputS: unit -> {file: File.t,
				      print: string -> unit,
				      done: unit -> unit},
		    docc: {input: File.t,
			   output: File.t} -> unit} -> unit
      val forceBasisLibrary: Dir.t -> unit
      val layoutBasisLibrary: unit -> Layout.t
      (* output a C file to print out the basis constants. *)
      val outputBasisConstants: Out.t -> unit
   end
