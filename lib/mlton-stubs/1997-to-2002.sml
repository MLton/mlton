structure OS =
   struct
      open OS
      structure FileSys =
	 struct
	    open FileSys
	    fun readDir d =
	       case FileSys.readDir d of
		  "" => NONE
		| s => SOME s
	 end
   end
