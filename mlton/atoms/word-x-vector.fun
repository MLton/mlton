(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor WordXVector (S: WORD_X_VECTOR_STRUCTS): WORD_X_VECTOR = 
struct

open S

datatype t = T of {elementSize: WordSize.t,
		   elements: WordX.t vector}

local
   fun make f (T r) = f r
in
   val elementSize = make #elementSize
   val elements = make #elements
end

fun toString (T {elements, elementSize}): string =
   let
      val n = Bits.toInt (WordSize.bits elementSize)
   in
      implode
      (rev
       (Vector.fold (elements, [], fn (w, ac) =>
		     let
			fun loop (i, w, ac) =
			   if i = 0
			      then ac
			   else
			      let
				 val (q, r) = IntInf.quotRem (w, 0x100)
			      in
				 loop (i - 8, q,
				       Char.fromInt (IntInf.toInt r) :: ac)
			      end
		     in
			loop (n, WordX.toIntInf w, ac)
		     end)))
   end
   
val layout = Layout.str o toString

fun equals (v, v') = Vector.equals (elements v, elements v', WordX.equals)

fun forall (v, f) = Vector.forall (elements v, f)
   
fun fromString s =
   T {elementSize = WordSize.byte,
      elements = Vector.tabulate (String.size s, fn i =>
				  WordX.fromChar (String.sub (s, i)))}
				  
fun length v = Vector.length (elements v)

fun sub (v, i) = Vector.sub (elements v, i)
   
fun tabulate ({elementSize}, n, f) =
   T {elementSize = elementSize,
      elements = Vector.tabulate (n, f)}
   
end
