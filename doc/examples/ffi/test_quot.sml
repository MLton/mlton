
val c_quot = _import "c_quot": Int8.int * Int8.int -> Int8.int;

val sml_quot = _export "sml_quot": (Int8.int * Int8.int -> Int8.int) -> unit;
val _ = sml_quot Int8.quot

val call_sml_quot = _import "call_sml_quot": unit -> unit;

val x : Int8.int = ~1
val y : Int8.int = 10

val z = Int8.quot (x, y)
val c_z = c_quot (x, y)

val bad_z =
   let
      val x : Int8.int = ~1
      val x : Word8.word = 0wxFF
      val x : Int32.int = Word8.toInt x
      val y : Int8.int = 10
      val y : Word8.word = 0wx0A
      val y : Int32.int = Word8.toInt y
      val z : Int32.int = Int32.quot (x, y)
      val z = Int8.fromInt z
   in
      z
   end

val () = 
   print (concat [" bad_z = ", Int8.toString bad_z, "\n",
                  "     z = ", Int8.toString z, "\n",
                  "   c_z = ", Int8.toString c_z, "\n"])
val () = call_sml_quot ()
