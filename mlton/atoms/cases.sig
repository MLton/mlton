type int = Int.t
type word = Word.t

signature CASES_STRUCTS = 
   sig
      type con
   end

signature CASES = 
   sig
      include CASES_STRUCTS
      
      datatype 'a t =
	 Char of (char * 'a) vector
       | Con of (con * 'a) vector
       | Int of (int * 'a) vector
       | Word of (word * 'a) vector
       | Word8 of (Word8.t * 'a) vector

      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b
      val forall: 'a t * ('a -> bool) -> bool
      val foreach: 'a t * ('a -> unit) -> unit
      val foreach': 'a t * ('a -> unit) * (con -> unit) -> unit
      val hd: 'a t -> 'a
      val isEmpty: 'a t -> bool
      val length: 'a t -> int
      val map: 'a t * ('a -> 'b) -> 'b t
   end

functor TestCasesVector (S: CASES) =
struct

open S

val _ = Assert.assert ("Cases", fn () => true)

end
