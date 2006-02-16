(* Copyright (C) 2003-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Real32 =
  Real
  (structure P = Primitive.Real32
   open P
   fun fromLarge m r =
      IEEEReal.withRoundingMode (m, fn () => P.fromLarge r)

   val realToWord: real -> word =
      fn r =>
      Word.fromLarge (PackWord32Little.subVec (PackReal32Little.toBytes r, 0))
         
   val wordToReal: word -> real =
      let
         val a = Word8Array.array (4, 0w0)
      in
         fn w =>
         let
            val _ = PackWord32Little.update (a, 0, Word.toLarge w)
         in
            PackReal32Little.subArr (a, 0)
         end
      end

   fun nextAfterUp r = wordToReal (Word.+ (realToWord r, 0w1))
   fun nextAfterDown r = wordToReal (Word.- (realToWord r, 0w1))
  )
