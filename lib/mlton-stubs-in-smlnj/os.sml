structure OS =
   struct
      open Pervasive.OS

      structure FileSys =
	 struct
	    open FileSys
	       
	    val fileSize = Pervasive.Int32.fromInt o fileSize
	    val hash = Pervasive.Word32.fromLargeWord o Pervasive.Word.toLargeWord o hash
	 end
   end
