(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure SMLofNJ: SML_OF_NJ =
   struct
      structure Cont =
	 struct
	    structure C = MLtonCont

	    type 'a cont = 'a C.t
	    val callcc = C.callcc
	    fun throw k v = C.throw (k, v)
	 end
      	 
      structure SysInfo =
	 struct
	    exception UNKNOWN
	    datatype os_kind = BEOS | MACOS | OS2 | UNIX | WIN32

	    fun getHostArch () =
	       let
		  open Primitive.MLton
	       in
		  case hostType of
		     Cygwin => "X86"
		   | FreeBSD => "X86"
		   | Linux => "X86"
		   | Sun => "SPARC"
	       end
		     
	    fun getOSKind () = UNIX
	    fun getOSName () =
	       let
		  open Primitive.MLton
	       in
		  case hostType of
		     Cygwin => "Cygwin"
		   | FreeBSD => "FreeBSD"
		   | Linux => "Linux"
		   | Sun => "Solaris"
	       end
	 end
      
      structure Internals =
	 struct
	    structure GC =
	       struct
		  fun messages _ = ()
	       end
	 end

      val getCmdName = CommandLine.name
      val getArgs = CommandLine.arguments

      fun getAllArgs () = getCmdName () :: getArgs ()

      val exnHistory = MLtonExn.history
	 
      structure World = MLtonWorld

      fun exportFn (file: string, f) =
	 let open MLtonWorld OS.Process
	 in case save (file ^ ".mlton") of
	    Original => exit success
	  | Clone => exit (f (getCmdName (), getArgs ()) handle _ => failure)
	 end

      fun exportML (f: string): bool =
	 let open MLtonWorld
	 in case save (f ^ ".mlton") of
	    Clone => true
	  | Original => false
	 end
   end
   
