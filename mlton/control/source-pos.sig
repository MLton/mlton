type int = Int.t
   
signature SOURCE_POS_STRUCTS = 
   sig
   end

signature SOURCE_POS = 
   sig
      include SOURCE_POS_STRUCTS
      
      datatype t = T of {column: int,
			 file: File.t,
			 line: int}

      val bogus: t
      val file: t -> File.t
      val toString: t -> string
   end


functor TestSourcePos (S: SOURCE_POS): sig end = 
struct

val _ = print "TestSourcePos\n"

open S

val _ = Assert.assert ("SourcePos", fn () => true)

end
