type int = Int.t
   
signature C_FUNCTION_STRUCTS = 
   sig
      structure CType: C_TYPE
   end

signature C_FUNCTION = 
   sig
      include C_FUNCTION_STRUCTS
      
      structure Convention:
	 sig
	    datatype t = Cdecl | Stdcall

	    val layout: t -> Layout.t
	    val toString: t -> string
	 end

      datatype t = T of {
			 args: CType.t vector,
			 (* bytesNeeded = SOME i means that the i'th
			  * argument to the function is a word that
			  * specifies the number of bytes that must be
			  * free in order for the C function to succeed.
			  * Limit check insertion is responsible for
			  * making sure that the bytesNeeded is available.
			  *)
			 bytesNeeded: int option,
			 convention: Convention.t,
			 ensuresBytesFree: bool,
			 mayGC: bool,
			 maySwitchThreads: bool,
			 modifiesFrontier: bool,
			 modifiesStackTop: bool,
			 name: string,
			 return: CType.t option}

      val allocTooLarge: t
      val args: t -> CType.t vector
      val bug: t
      val bytesNeeded: t -> int option
      val ensuresBytesFree: t -> bool
      val equals: t * t -> bool
      val gc: {maySwitchThreads: bool} -> t
      val isOk: t -> bool
      val layout: t -> Layout.t
      val mayGC: t -> bool
      val maySwitchThreads: t -> bool
      val modifiesFrontier: t -> bool
      val modifiesStackTop: t -> bool
      val name: t -> string
      val profileEnter: t
      val profileInc: t
      val profileLeave: t
      val prototype: t -> string
      (* returnToC is not really a C function.  Calls to it must be handled
       * specially by each codegen to ensure that the C stack is handled
       * correctly.  However, for the purposes of everything up to the
       * backend it looks like a call to C.
       *)
      val returnToC: t
      val return: t -> CType.t option
      val size: t
      val vanilla: {args: CType.t vector,
		    name: string,
		    return: CType.t option} -> t
   end
