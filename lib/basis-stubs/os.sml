
structure OS =
   struct
      open OS
	
      structure FileSys =
	 struct
	    open FileSys

	    val readDir = fn d =>
	       case readDir d of
		  "" => NONE
		| s => SOME s
	 end
   end
