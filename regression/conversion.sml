val w8t16  = Word16.fromLarge o Word8.toLarge
val w16t32 = Word32.fromLarge o Word16.toLarge
val w32t64 = Word64.fromLarge o Word32.toLarge

(* All of these should become no-ops *)
val id8_1 = Word8.fromLarge o Word8.toLarge
val id8_2 = Word8.fromLarge o Word8.toLargeX
val id8_3 = Word8.fromLarge o Word16.toLarge  o Word16.fromLarge o Word8.toLarge
val id8_4 = Word8.fromLarge o Word16.toLargeX o Word16.fromLarge o Word8.toLargeX
val id8_5 = Word8.fromLarge o Word16.toLarge  o Word16.fromLarge o Word8.toLargeX

val id16_1 = Word16.fromLarge o Word16.toLarge
val id16_2 = Word16.fromLarge o Word16.toLargeX
val id16_3 = Word16.fromLarge o Word32.toLarge  o Word32.fromLarge o Word16.toLarge
val id16_4 = Word16.fromLarge o Word32.toLarge  o Word32.fromLarge o Word16.toLargeX
val id16_5 = Word16.fromLarge o Word32.toLargeX o Word32.fromLarge o Word16.toLarge
val id16_6 = Word16.fromLarge o Word32.toLargeX o Word32.fromLarge o Word16.toLargeX
val xx16_1 = Word16.fromLarge o Word8.toLarge   o Word8.fromLarge  o Word16.toLarge
val xx16_2 = Word16.fromLarge o Word8.toLarge   o Word8.fromLarge  o Word16.toLargeX
val xx16_3 = Word16.fromLarge o Word8.toLargeX  o Word8.fromLarge  o Word16.toLarge
val xx16_4 = Word16.fromLarge o Word8.toLargeX  o Word8.fromLarge  o Word16.toLargeX

val id32_1 = Word32.fromLarge o Word32.toLarge
val id32_2 = Word32.fromLarge o Word32.toLargeX
val id32_3 = Word32.fromLarge o Word64.toLarge  o Word64.fromLarge o Word32.toLarge
val id32_4 = Word32.fromLarge o Word64.toLarge  o Word64.fromLarge o Word32.toLargeX
val id32_5 = Word32.fromLarge o Word64.toLargeX o Word64.fromLarge o Word32.toLarge
val id32_6 = Word32.fromLarge o Word64.toLargeX o Word64.fromLarge o Word32.toLargeX
val xx32_1 = Word32.fromLarge o Word8.toLarge   o Word8.fromLarge  o Word32.toLarge
val xx32_2 = Word32.fromLarge o Word8.toLarge   o Word8.fromLarge  o Word32.toLargeX
val xx32_3 = Word32.fromLarge o Word16.toLargeX o Word16.fromLarge o Word32.toLarge
val xx32_4 = Word32.fromLarge o Word16.toLargeX o Word16.fromLarge o Word32.toLargeX

val id64_1 = Word64.fromLarge o Word64.toLarge
val id64_2 = Word64.fromLarge o Word64.toLargeX
val id64_3 = Word64.fromLarge o Word64.toLarge  o Word64.fromLarge o Word64.toLarge
val id64_4 = Word64.fromLarge o Word64.toLarge  o Word64.fromLarge o Word64.toLargeX
val id64_5 = Word64.fromLarge o Word64.toLargeX o Word64.fromLarge o Word64.toLarge
val id64_6 = Word64.fromLarge o Word64.toLargeX o Word64.fromLarge o Word64.toLargeX
val xx64_1 = Word64.fromLarge o Word32.toLarge  o Word32.fromLarge o Word64.toLarge
val xx64_2 = Word64.fromLarge o Word32.toLarge  o Word32.fromLarge o Word64.toLargeX
val xx64_3 = Word64.fromLarge o Word16.toLargeX o Word16.fromLarge o Word64.toLarge
val xx64_4 = Word64.fromLarge o Word16.toLargeX o Word16.fromLarge o Word64.toLargeX

val c8t16_1 = Word16.fromLarge o Word8.toLarge
val c8t16_2 = Word16.fromLarge o Word8.toLargeX
val c8t16_3 = Word16.fromLarge o Word8.toLarge o id8_3
val c8t16_4 = Word16.fromLarge o Word8.toLarge o id8_4
val c8t16_5 = Word16.fromLarge o Word8.toLarge o id8_5

val c8t32_1 = Word32.fromLarge o Word16.toLarge  o Word16.fromLarge o Word8.toLarge
val c8t32_2 = Word32.fromLarge o Word16.toLarge  o Word16.fromLarge o Word8.toLargeX
val c8t32_3 = Word32.fromLarge o Word16.toLargeX o Word16.fromLarge o Word8.toLarge
val c8t32_4 = Word32.fromLarge o Word16.toLargeX o Word16.fromLarge o Word8.toLargeX

val c8t64_1 = Word64.fromLarge o Word16.toLarge  o Word16.fromLarge o Word8.toLarge
val c8t64_2 = Word64.fromLarge o Word16.toLarge  o Word16.fromLarge o Word8.toLargeX
val c8t64_3 = Word64.fromLarge o Word16.toLargeX o Word16.fromLarge o Word8.toLarge
val c8t64_4 = Word64.fromLarge o Word16.toLargeX o Word16.fromLarge o Word8.toLargeX
val c8t64_5 = Word64.fromLarge o Word8.toLarge  o Word8.fromLarge o Word16.toLarge o Word16.fromLarge o Word8.toLarge
val c8t64_6 = Word64.fromLarge o Word8.toLargeX o Word8.fromLarge o Word16.toLarge o Word16.fromLarge o Word8.toLargeX

val c16t8_1 = Word8.fromLarge o Word16.toLarge
val c16t8_2 = Word8.fromLarge o Word16.toLargeX
val c16t8_3 = Word8.fromLarge o Word32.toLarge o Word32.fromLarge o Word16.toLargeX
val c16t8_4 = id8_3 o Word8.fromLarge o Word16.toLargeX
val c16t8_5 = id8_4 o Word8.fromLarge o Word16.toLarge
val c16t8_6 = id8_5 o Word8.fromLarge o Word16.toLargeX

(* These tests rely on Int = Int32 to be useful *)
val w16t8z = Word8.fromInt o Word16.toInt
val w16t8s = Word8.fromInt o Word16.toIntX

val c16t32_1 = Word32.fromLarge o Word8.toLargeX o Word8.fromLarge o Word16.toLarge
val c16t32_2 = Word32.fromLarge o Word8.toLarge o Word8.fromLarge o Word16.toLargeX
val c16t32_3 = Word32.fromLarge o Word16.toLarge
val c16t32_4 = Word32.fromLarge o Word16.toLargeX

val c16t64_1 = Word64.fromLarge o Word32.toLarge  o Word32.fromLarge o Word16.toLarge
val c16t64_2 = Word64.fromLarge o Word32.toLarge  o Word32.fromLarge o Word16.toLargeX
val c16t64_3 = Word64.fromLarge o Word32.toLargeX o Word32.fromLarge o Word16.toLarge
val c16t64_4 = Word64.fromLarge o Word32.toLargeX o Word32.fromLarge o Word16.toLargeX
val c16t64_5 = Word64.fromLarge o Word8.toLarge  o Word8.fromLarge o Word16.toLarge
val c16t64_6 = Word64.fromLarge o Word8.toLarge  o Word8.fromLarge o Word16.toLargeX
val c16t64_7 = Word64.fromLarge o Word8.toLargeX o Word8.fromLarge o Word16.toLarge
val c16t64_8 = Word64.fromLarge o Word8.toLargeX o Word8.fromLarge o Word16.toLargeX

val c32t8_1 = id8_4 o Word8.fromLarge o Word32.toLarge  o id32_1
val c32t8_2 = id8_3 o Word8.fromLarge o Word32.toLargeX o id32_2
val c32t8_3 = id8_2 o Word8.fromLarge o Word32.toLarge  o id32_3
val c32t8_4 = id8_1 o Word8.fromLarge o Word32.toLargeX o id32_4

val c32t16_1 = Word16.fromLarge o Word8.toLarge  o Word8.fromLarge o Word32.toLarge
val c32t16_2 = Word16.fromLarge o Word8.toLarge  o Word8.fromLarge o Word32.toLargeX
val c32t16_3 = Word16.fromLarge o Word8.toLargeX o Word8.fromLarge o Word32.toLarge
val c32t16_4 = Word16.fromLarge o Word8.toLargeX o Word8.fromLarge o Word32.toLargeX
val c32t16_5 = Word16.fromLarge o Word64.toLarge  o Word64.fromLarge o Word32.toLarge
val c32t16_6 = Word16.fromLarge o Word64.toLargeX o Word64.fromLarge o Word32.toLarge
val c32t16_7 = Word16.fromLarge o Word64.toLarge  o Word64.fromLarge o Word32.toLargeX
val c32t16_8 = Word16.fromLarge o Word64.toLargeX o Word64.fromLarge o Word32.toLargeX

val c64t8_1 = id8_4 o Word8.fromLarge o Word64.toLarge  o id64_1
val c64t8_2 = id8_3 o Word8.fromLarge o Word64.toLargeX o id64_2
val c64t8_3 = id8_2 o Word8.fromLarge o Word64.toLarge  o id64_3
val c64t8_4 = id8_1 o Word8.fromLarge o Word64.toLargeX o id64_4

val c64t16_1 = Word16.fromLarge o Word8.toLarge  o Word8.fromLarge o Word64.toLarge
val c64t16_2 = Word16.fromLarge o Word8.toLarge  o Word8.fromLarge o Word64.toLargeX
val c64t16_3 = Word16.fromLarge o Word8.toLargeX o Word8.fromLarge o Word64.toLarge
val c64t16_4 = Word16.fromLarge o Word8.toLargeX o Word8.fromLarge o Word64.toLargeX
val c64t16_5 = Word16.fromLarge o Word32.toLarge  o Word32.fromLarge o Word64.toLarge
val c64t16_6 = Word16.fromLarge o Word32.toLargeX o Word32.fromLarge o Word64.toLarge
val c64t16_7 = Word16.fromLarge o Word32.toLarge  o Word32.fromLarge o Word64.toLargeX
val c64t16_8 = Word16.fromLarge o Word32.toLargeX o Word32.fromLarge o Word64.toLargeX

val f8t8   = [ id8_1, id8_2, id8_3, id8_4, id8_5 ]
val f8t16  = [ c8t16_1, c8t16_2, c8t16_3, c8t16_4, c8t16_5 ]
val f8t32  = [ c8t32_1, c8t32_2, c8t32_3, c8t32_4 ]
val f8t64  = [ c8t64_1, c8t64_2, c8t64_3, c8t64_4, c8t64_5, c8t64_6 ]

val f16t8  = [ c16t8_1, c16t8_2, c16t8_3, c16t8_5, c16t8_6, w16t8z, w16t8s ]
val f16t16 = [ id16_1, id16_2, id16_3, id16_4, id16_5, id16_6, xx16_1, xx16_2, xx16_3, xx16_4 ]
val f16t32 = [ c16t32_1, c16t32_2, c16t32_3, c16t32_4 ]
val f16t64 = [ c16t64_1, c16t64_2, c16t64_3, c16t64_4, c16t64_6, c16t64_7, c16t64_8 ]

val f32t8  = [ c32t8_1, c32t8_2, c32t8_3, c32t8_4 ]
val f32t16 = [ c32t16_1, c32t16_2, c32t16_3, c32t16_4, c32t16_5, c32t16_6, c32t16_7, c32t16_8 ]
val f32t32 = [ id32_1, id32_2, id32_3, id32_4, id32_5, id32_6, xx32_1, xx32_2, xx32_3, xx32_4 ]
val f32t64 = [ ]

val f64t8  = [ c64t8_1, c64t8_2, c64t8_3, c64t8_4 ]
val f64t16 = [ c64t16_1, c64t16_2, c64t16_3, c64t16_4, c64t16_5, c64t16_6, c64t16_7, c64t16_8 ]
val f64t32 = [ ]
val f64t64 = [ id64_1, id64_2, id64_3, id64_4, id64_5, id64_6, xx64_1, xx64_2, xx64_3, xx64_4 ]

val x8  = [ 0w0, 0w1, 0w2, 0w3, 0wx7f, 0wx7e, 0wxfe, 0wxff ]
val x16 = [ 0wx7fff, 0wxfffe, 0wxffff ] @ List.map w8t16 x8
val x32 = [ 0wx7fffffff, 0wxfffffffe, 0wxffffffff ] @ List.map w16t32 x16
val x64 = [ 0wx7fffffffffffffff, 0wxfffffffffffffffe, 0wxffffffffffffffff ] @ List.map w32t64 x32

fun doit (out, xl) f =
   let
      val () = List.app (print o out o f) xl
   in
      print "\n"
   end

val () = List.app (doit (Word8.toString, x8))  f8t8
val () = List.app (doit (Word8.toString, x16)) f16t8
val () = List.app (doit (Word8.toString, x32)) f32t8
val () = List.app (doit (Word8.toString, x64)) f64t8

val () = List.app (doit (Word16.toString, x8))  f8t16
val () = List.app (doit (Word16.toString, x16)) f16t16
val () = List.app (doit (Word16.toString, x32)) f32t16
val () = List.app (doit (Word16.toString, x64)) f64t16

val () = List.app (doit (Word32.toString, x8))  f8t32
val () = List.app (doit (Word32.toString, x16)) f16t32
val () = List.app (doit (Word32.toString, x32)) f32t32
val () = List.app (doit (Word32.toString, x64)) f64t32

val () = List.app (doit (Word64.toString, x8))  f8t64
val () = List.app (doit (Word64.toString, x16)) f16t64
val () = List.app (doit (Word64.toString, x32)) f32t64
val () = List.app (doit (Word64.toString, x64)) f64t64
