functor FlagsConvert
        (structure Flags: BIT_FLAGS) :
        POSIX_FLAGS_1997 where type flags = Flags.flags =
  struct
     open Flags
     val wordTo = fromWord
  end