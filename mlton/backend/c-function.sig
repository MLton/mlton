(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

type int = Int.t
type word = Word.t
   
signature C_FUNCTION_STRUCTS =
   sig
      structure Type: MTYPE
   end

signature C_FUNCTION =
   sig
      include C_FUNCTION_STRUCTS

      type t
	 
      val bug: t
      val bytesNeeded: t -> int option
      val dest: t -> {bytesNeeded: int option,
		      ensuresBytesFree: bool,
		      modifiesFrontier: bool,
		      modifiesStackTop: bool,
		      mayGC: bool,
		      maySwitchThreads: bool,
		      name: string,
		      returnTy: Type.t option}
      val ensuresBytesFree: t -> bool
      val equals: t * t -> bool
      val gc: {maySwitchThreads: bool} -> t
      val isOk: t -> bool
      val layout: t -> Layout.t
      val make: {(* bytesNeeded = SOME i means that the i'th
		  * argument to the function is a word that
		  * specifies the number of bytes that must be
		  * free in order for the C function to succeed.
		  * Limit check insertion is responsible for
		  * making sure that the bytesNeeded is available.
		  *)
		 bytesNeeded: int option,
		 ensuresBytesFree: bool,
		 modifiesFrontier: bool,
		 modifiesStackTop: bool,
		 mayGC: bool,
		 maySwitchThreads: bool,
		 name: string,
		 returnTy: Type.t option} -> t
      val mayGC: t -> bool
      val maySwitchThreads: t -> bool
      val modifiesFrontier: t -> bool
      val modifiesStackTop: t -> bool
      val name: t -> string
      val profileEnter: t
      val profileInc: t
      val profileLeave: t
      (* returnToC is not really a C function.  Calls to it must be handled
       * specially by each codegen to ensure that the C stack is handled
       * correctly.  However, for the purposes of the backend it looks like a
       * call to C.
       *)
      val returnToC: t
      val returnTy: t -> Type.t option
      val size: t
      val vanilla: {name: string, returnTy: Type.t option} -> t
   end
