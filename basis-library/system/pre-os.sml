structure OS =
   struct
      type syserror = PosixPrimitive.Error.syserror
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
