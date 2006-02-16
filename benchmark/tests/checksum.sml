(* Author: sweeks@sweeks.com
 * This code is based on the following paper.
 * The Performance of FoxNet 2.0
 * Herb Derby
 * CMU-CS-99-137
 * June 1999
 *)

fun checkOne (new, ac) =
   let open Word32
   in ac + >> (new, 0w16) + andb (new, 0wxFFFF)
   end

fun fold f b (buf, first, last) =
   let
      fun loop (i, ac) =
         if i > last
            then ac
         else loop (i + 1,
                    f (Word32.fromLarge (PackWord32Little.subArr (buf, i)),
                       ac))
   in
      loop (first, b)
   end

fun checksum buf = fold checkOne 0w0 buf

structure Main =
   struct
      fun doit n =
         let
            val first = 0
            val size = 10000000
            val buf = Word8Array.array (size, 0w0)
            val bytesPerWord = 4
            val last = size div bytesPerWord - 1
            val rec loop =
               fn 0 => ()
                | n =>
                     let val w = checksum (buf, first, last)
                        val _ = if w <> 0w0 then raise Fail "bug" else ()
                     in loop (n - 1)
                     end
         in loop n
         end
   end
