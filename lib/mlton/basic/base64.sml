(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Base64: BASE64 =
   struct
      val chars =
         "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

      fun word8ToChar(w: Word8.t): char =
         String.sub(chars, Word8.toInt w)

      val word8ToChar =
         Trace.traceAssert
         ("Base64.word8ToChar", Word8.layout, Char.layout,
          fn w => (0w0 <= w andalso w < 0w64, fn _ => true))
         word8ToChar

      val charToWord8: char -> Word8.t option =
         Char.memoize(fn c =>
                      Option.map(String.peeki(chars, fn (_, c') => c = c'),
                                 fn (i, _) => Word8.fromInt i))

      val charToWord8 =
         Trace.trace("Base64.charToWord8",
                     Char.layout, Option.layout Word8.layout)
         charToWord8

      val pad = #"="

      fun 'a encodeGen{array: 'a,
                       length: 'a -> int,
                       sub: 'a * int -> Word8.t} =
         let
            val n = length array
            val sub = fn i => sub(array, i)
            val (d, m) = Int.divMod(n, 3)
            val (d, n') = if m = 0
                             then (d, n)
                          else if m = 1
                                  then (d + 1, n + 2)
                               else (d + 1, n + 1)
            val sub = fn i => if i >= n then 0w0 else sub i
            val _ = Assert.assert("Base64.encodeGen", fn () => n' mod 3 = 0)
            val numChars: int = 4 * d
            val chars = CharArray.array(numChars, #"\000")
            fun updateChar(j, c) = CharArray.update(chars, j, c)
            fun update(j: int, w: Word8.t) = updateChar(j, word8ToChar w)
            fun loop(i: int, j: int) =
               if i = n'
                  then j
               else
                  let val w1 = sub i
                     val w2 = sub(i + 1)
                     val w3 = sub(i + 2)
                     open Word8
                     val op + = Int.+
                  in update(j, >>(w1, 0w2))
                     ; update(j + 1, orb(<<(andb(w1, 0w3), 0w4),
                                         >>(w2, 0w4)))
                     ; update(j + 2, orb(<<(andb(w2, 0wxF), 0w2),
                                         >>(w3, 0w6)))
                     ; update(j + 3, andb(w3, 0wx3F))
                     ; loop(i + 3, j + 4)
                  end
            val j = loop(0, 0)
            (* insert padding *)
            val _ = if m = 0
                       then ()
                    else (updateChar(j - 1, pad)
                          ; if m = 1
                               then updateChar(j - 2, pad)
                            else ())
         (* need to patch for leftover bits *)
         in String.fromCharArray chars
         end

      fun encode s = encodeGen{array = s,
                               length = String.size,
                               sub = Char.toWord8 o String.sub}

      val encode =
         Trace.trace("Base64.encode", String.layout, String.layout) encode

      fun 'a decodeGen{string: string,
                       new: int * Word8.t -> 'a,
                       update: 'a * int * Word8.t -> unit}: 'a =
         let
            val n = String.size string
            val (d, m) = Int.divMod(n, 4)
            val _ = Assert.assert("Base64.decodeGen", fn () => m = 0)
            fun sub i = String.sub(string, i)
            val numPads =
               if pad = sub(n - 1)
                  then if pad = sub(n - 2)
                          then 2
                       else 1
               else 0
            val outputLength = d * 3 - numPads
            val a = new(outputLength, 0w0)
            fun loop(i: int, j: int): unit =
               if i = n
                  then ()
               else
                  let
                     val sub =
                        fn i =>
                        let val c = sub i
                        in if pad = c
                              then 0w0
                           else
                              case charToWord8 c of
                                 NONE =>
                                    Error.bug (concat
                                               ["Base64.decodeGen: strange char ",
                                                Char.escapeSML c])
                               | SOME w => w
                        end
                     val w0 = sub i
                     val w1 = sub(i + 1)
                     val w2 = sub(i + 2)
                     val w3 = sub(i + 3)
                     val update =
                        fn (k, w) =>
                        if j + k >= outputLength
                           then ()
                        else update(a, j + k, w)
                     val _ =
                        let open Word8
                        in update(0, orb(<<(w0, 0w2), >>(w1, 0w4)))
                           ; update(1, orb(<<(andb(w1, 0wxF), 0w4), >>(w2, 0w2)))
                           ; update(2, orb(<<(andb(w2, 0w3), 0w6), w3))
                        end
                  in loop(i + 4, j + 3)
                  end
            val _ = loop(0, 0)
         in a
         end

      fun decode s =
         String.fromCharArray
         (decodeGen
          {string = s,
           new = fn (n, w) => CharArray.array(n, Char.fromWord8 w),
           update = fn (a, i, w) => CharArray.update(a, i, Char.fromWord8 w)})

      val decode =
         Trace.trace("Base64.decode", String.layout, String.layout) decode
   end
