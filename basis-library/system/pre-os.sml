
structure OS =
  struct
    type syserror = PosixPrimitive.Error.syserror
    structure Process =
      struct
	type status = PosixPrimitive.Process.status
      end
    structure IO = 
      struct
	datatype iodesc = datatype PosixPrimitive.IO.file_desc
      end
  end

structure PreOS = OS