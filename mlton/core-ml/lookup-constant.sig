(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
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

      val build: CoreML.Dec.t vector * Out.t -> unit
      val load: CoreML.Dec.t vector * In.t -> string -> Const.t
   end


functor TestLookupConstant (S: LOOKUP_CONSTANT): sig end = 
struct

open S

val _ = Assert.assert("LookupConstant", fn () => true)

end
