(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
signature COMPILE =
   sig
      (* output a C file to print out the basis constants. *)
      val outputBasisConstants: Out.t -> unit
      val compile: {input: File.t list,
		    outputC: unit -> {file: File.t,
				      print: string -> unit,
				      done: unit -> unit},
		    outputS: unit -> {file: File.t,
				      print: string -> unit,
				      done: unit -> unit},
		    docc: {input: File.t,
			   output: File.t} -> unit} -> unit
      val layoutBasisLibrary: unit -> Layout.t
      val setBasisLibraryDir: Dir.t -> unit
   end
