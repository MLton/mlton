functor IntSize (S: INT_SIZE_STRUCTS): INT_SIZE =
struct

datatype t = I8 | I16 | I32 | I64

val equals: t * t -> bool = op =

val all = [I8, I16, I32, I64]

val default = I32

val bytes: t -> int =
   fn I8 => 1
    | I16 => 2
    | I32 => 4
    | I64 => 8
   
fun size s = 8 * bytes s

val toString = Int.toString o size

val layout = Layout.str o toString

val memoize: (t -> 'a) -> t -> 'a =
   fn f =>
   let
      val a8 = f I8
      val a16 = f I16
      val a32 = f I32
      val a64 = f I64
   in
      fn I8 => a8
       | I16 => a16
       | I32 => a32
       | I64 => a64
   end

val range =
   memoize
   (fn s =>
    let
       val pow = IntInf.pow (IntInf.fromInt 2, size s - 1)
    in
       (IntInf.~ pow, IntInf.- (pow, IntInf.fromInt 1))
    end)

fun isInRange (s, i) =
   let
      val (min, max) = range s
   in
      IntInf.<= (min, i) andalso IntInf.<= (i, max)
   end

val min = #1 o range

val max = #2 o range

end
