(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor Mtype (S: MTYPE_STRUCTS): MTYPE = 
struct

open S

datatype t =
   Int of IntSize.t
 | Pointer
 | Real of RealSize.t
 | Word of WordSize.t

datatype dest = datatype t

fun dest t = t

val isReal =
   fn Real _ => true
    | _ => false

fun memo f =
   let
      val int = IntSize.memoize (f o Int)
      val pointer = f Pointer
      val real = RealSize.memoize (f o Real)
      val word = WordSize.memoize (f o Word)
   in
      fn Int s => int s
       | Pointer => pointer
       | Real s => real s
       | Word s => word s
   end

val toString =
   memo
   (fn t =>
    case t of
       Int s => concat ["Int", IntSize.toString s]
     | Pointer => "Pointer"
     | Real s => concat ["Real", RealSize.toString s]
     | Word s => concat ["Word", WordSize.toString s])

val layout = Layout.str o toString

fun equals (t, t') = t = t'

val equals =
   Trace.trace2 ("Runtime.Type.equals", layout, layout, Bool.layout) equals

val int = IntSize.memoize Int
val pointer = Pointer
val real = RealSize.memoize Real
val word = WordSize.memoize Word

val all =
   List.map (IntSize.all, int)
   @ [pointer]
   @ List.map (RealSize.all, real)
   @ List.map (WordSize.all, word)

val bool = int IntSize.I32

val defaultInt = int IntSize.default

val defaultReal = real RealSize.default
   
val defaultWord = word WordSize.default

val label = word WordSize.W32
  
fun isPointer t =
   case t of
      Pointer => true
    | _ => false

fun size (t: t): int =
   case t of
      Int s => IntSize.bytes s
    | Pointer => 4
    | Real s => RealSize.bytes s
    | Word s => WordSize.bytes s

fun name t =
   case t of
      Int s => concat ["I", IntSize.toString s]
    | Pointer => "P"
    | Real s => concat ["R", RealSize.toString s]
    | Word s => concat ["W", WordSize.toString s]

local
   fun align a b =
      let
	 open Word
	 val a = fromInt a - 0w1
      in
	 toInt (andb (notb a, a + fromInt b))
      end
in
   val align4 = align 4
   val align8 = align 8
   val align: t * int -> int = fn (ty, n) => align (size ty) n
end

end
