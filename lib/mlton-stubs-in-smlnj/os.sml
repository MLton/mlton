structure OS =
   struct
      open OpenInt32 OS

      structure FileSys =
	 struct
	    open FileSys
	       
	    val fileSize = fromInt o fileSize
	 end
   end
