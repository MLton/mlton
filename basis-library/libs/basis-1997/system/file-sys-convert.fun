functor OSFileSysConvert
        (structure FileSys : OS_FILE_SYS) :
        OS_FILE_SYS_1997 =
  struct
     open FileSys
     val readDir = fn d =>
       case readDir d of
	 NONE => ""
       | SOME s => s
  end
