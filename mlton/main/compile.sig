(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
type int = Int.t

signature COMPILE_STRUCTS =
   sig
   end

signature COMPILE =
   sig
      include COMPILE_STRUCTS
	 
      val compile: {input: File.t list,
		    outputC: unit -> {file: File.t,
				      print: string -> unit,
				      done: unit -> unit},
		    outputS: unit -> {file: File.t,
				      print: string -> unit,
				      done: unit -> unit}} -> unit
      val forceBasisLibrary: Dir.t -> unit
      val layoutBasisLibrary: unit -> Layout.t
      (* output a C file to print out the basis constants. *)
      val outputBasisConstants: Out.t -> unit
      val setBasisLibraryDir: Dir.t -> unit
      val typeCheck: {input: File.t list} -> unit
   end
