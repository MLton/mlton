functor OSConvert
        (structure OS: OS) :
        OS_1997 = 
  struct
     open OS
     structure FileSys = OSFileSysConvert(structure FileSys = FileSys)
     structure Path = OSPathConvert(structure Path = Path)
     structure Process = OSProcessConvert(structure Process = Process)
  end