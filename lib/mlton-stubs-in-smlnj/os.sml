structure OS =
   struct
      open OS

      structure FileSys =
	 struct
	    open FileSys
	       
	    val fileSize = Pervasive.Int32.fromInt o fileSize
	 end
   end
