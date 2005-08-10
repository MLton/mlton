(* Copyright (C) 2002-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure OS =
   struct
      structure Process =
	 struct
	    type status = PosixPrimitive.Process.Status.t
	 end
      structure IO :> sig
			 eqtype iodesc

			 val fromFD: PosixPrimitive.IO.file_desc -> iodesc
			 val toFD: iodesc -> PosixPrimitive.IO.file_desc
		      end = 
		      struct
			 type iodesc = PosixPrimitive.IO.file_desc

			 val fromFD = fn z => z
			 val toFD = fn z => z
		      end
   end

structure PreOS = OS
