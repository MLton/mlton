functor CType (S: C_TYPE_STRUCTS): C_TYPE = 
struct

open S

datatype z = datatype WordSize.t

datatype t =
   Int of IntSize.t
 | Pointer
 | Real of RealSize.t
 | Word of WordSize.t

val bool = Int (IntSize.I 32)
val char = Word W8
val defaultInt = Int IntSize.default
val defaultReal = Real RealSize.default
val defaultWord = Word WordSize.default
val pointer = Pointer

val all =
   List.map (IntSize.prims, Int)
   @ [Pointer]
   @ List.map (RealSize.all, Real)
   @ List.map (WordSize.all, Word)

val equals: t * t -> bool =
   fn (Int s, Int s') => IntSize.equals (s, s')
    | (Pointer, Pointer) => true
    | (Real s, Real s') => RealSize.equals (s, s')
    | (Word s, Word s') => WordSize.equals (s, s')
    | _ => false

val isPointer: t -> bool =
   fn Pointer => true
    | _ => false
   
fun memo (f: t -> 'a): t -> 'a =
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
   (fn u =>
    case u of
       Int s => concat ["Int", IntSize.toString s]
     | Pointer => "Pointer"
     | Real s => concat ["Real", RealSize.toString s]
     | Word s => concat ["Word", WordSize.toString s])

val layout = Layout.str o toString

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
