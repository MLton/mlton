structure OS =
   struct
      open Pervasive.OS

      structure FileSys =
	 struct
	    open FileSys
	       
	    val fileSize = Pervasive.Int32.fromInt o fileSize
	 end
   end
