type int = Int.t
type word = Word.t
   
signature LOOKUP_CONSTANT_STRUCTS = 
   sig
      structure CoreML: CORE_ML
   end

signature LOOKUP_CONSTANT = 
   sig
      include LOOKUP_CONSTANT_STRUCTS

      structure Const:
	 sig
	    datatype t =
	       Bool of bool
	     | Int of int
	     | Real of string
	     | String of string
	     | Word of word
	 end

      val build:
	 CoreML.Program.t * ({input: File.t, output: File.t} -> unit)
	 -> string -> Const.t
   end


functor TestLookupConstant (S: LOOKUP_CONSTANT): sig end = 
struct

open S

val _ = Assert.assert("LookupConstant", fn () => true)

end
