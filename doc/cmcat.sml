(*
 * Author: Stephen Weeks (sweeks@acm.org)
 *
 * This requires that you have SML/NJ installed.
 * It works with SML/NJ 110.9.1, and may require changes to work with other
 * versions, since it depends on the CM structure.
 *
 * cmcat takes a ".cm" file and prints on stdout a list of files in the order
 * deduced by CM.
 *
 * To use from the REPL, do the following:
 * Cmcat.cmcat {sources = "sources.cm",
 *	        out = TextIO.stdOut}
 *
 * Before using from the shell, you must do the following, where <smlnj> is
 * the root directory of the SML/NJ installation.  You may need to be root in
 * order to do these.
 * 1. From the SML/NJ REPL, call "export ();".  This creates a heap image
 *    "cmcat.x86-linux" in the local directory.
 * 2. ln -s <smlnj>/bin/.run-sml <smlnj>/bin/cmcat
 * 3. mv cmcat.x86-linux <smlnj>/bin/.heap
 *
 * Once it is installed, the usage is as follows:
 *   cmcat [-Dsym ...] sources.cm
 *
 * -Dsym can be used to define CM preprocessor symbols.
 *)

structure Cmcat:
   sig
      val cmcat: {
		  (* CM preprocessor defines *)
		  defines: string list,
		  (* The ".cm" filename *)
		  sources: string,
		  (* Where to write the output *)
		  out: TextIO.outstream
		 } -> unit

      (* Creates a (big) heap file. *)
      val export: unit -> unit
   end =
   struct
      open TextIO OS
      open FileSys Path Process
	 
      fun 'a dynamicWind (thunk: unit -> 'a, cleanup: unit -> unit): 'a =
	 let val a = thunk ()
	 in cleanup (); a
	 end handle exn => (cleanup (); raise exn)
	    
      local
	 fun ('stream, 'a) withh (file: string,
				 f: 'stream -> 'a,
				 openn: string -> 'stream,
				 close: 'stream -> unit): 'a =
	    let val stream = openn file
	    in dynamicWind (fn () => f stream,
			   fn () => close stream)
	    end
      in
	 fun 'a withOut (file: string, f: outstream -> 'a): 'a =
	    withh (file, f, openOut, closeOut)
      end

      fun 'a fluidLet (out: outstream, out': outstream, thunk: unit -> 'a): 'a =
	 let val old = getOutstream out
	 in dynamicWind
	    (fn () => (setOutstream (out, getOutstream out'); thunk ()),
	     fn () => setOutstream (out, old))
	 end
      
      fun 'a ignore (out: outstream, thunk: unit -> 'a): 'a =
	 withOut ("/dev/null", fn out' => fluidLet (out, out', thunk))

      fun 'a inTemp (thunk: unit -> 'a): 'a =
	 let val cur = getDir ()
	    val dir = tmpName ()
	 in mkDir dir
	    ; (dynamicWind
	       (fn () => (chDir dir; thunk ()),
		fn () => (system ("/bin/rm -r " ^ dir); chDir cur)))
	 end

      fun cmcat {defines, out, sources} =
	 let
	    (* Define preprocessor symbols *)
            val _ = List.app (fn n => CM.SymVal.define (n, 1)) defines
	    val dir = getDir ()
	 in
	    CM.verbose (SOME false)
	    ; inTemp (fn () =>
		      List.app (fn f =>
				let
				   val f = mkRelative {path = f,
						       relativeTo = dir}
				in
				   output (out, f ^ "\n")
				end)
		      (ignore (stdErr, fn () => CM.names' sources)))
	 end

      fun message s = output (stdErr, s ^ "\n")
   
      fun die msg =
	 (message "Usage: cmcat sources.cm"
	  ; message ("Error: " ^ msg)
	  ; exit failure)

      fun export () =
	 SMLofNJ.exportFn
	 ("cmcat", fn (_, args) =>
	  let
	     val defines = ref ["MLton"]
	     fun loop args = 
		case args of
		   [file] =>
		      cmcat {
			     defines = !defines,
			     out = stdOut,
			     (* Some versions of SML/NJ have a different type
			      * for mkAbsolute, which will cause a type error
			      * here.  If you get such an error, try the
			      * following: mkAbsolute (file, getDir ())
			      *)
			     sources = mkAbsolute {path = file,
						   relativeTo = getDir ()}}
	       | flag :: args =>
		    if String.isPrefix "-D" flag
		       then
			  (defines := String.extract (flag, 2, NONE) :: !defines
			   ; loop args)
		    else die (String.concat ["invalid flag ", flag])
	       | _ => die "wrong number of arguments"
	  in
	     loop args handle _ => die "cmcat failed"
	     ; 0
	  end)
   end


