(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure IntInf: INT_INF_EXTRA =
   struct
      open Primitive.IntInf
      type t = int

      structure BigWord = C_MPLimb
      structure SmallInt = ObjptrInt

      structure W = ObjptrWord
      structure I = ObjptrInt
      structure MPLimb = C_MPLimb

      val precision: Int.int option = NONE

      fun sign (arg: int): Int.int =
         if Prim.isSmall arg
            then I.sign (Prim.dropTagCoerceInt arg)
            else if isNeg arg
                    then ~1
                    else 1

      fun sameSign (x, y) = sign x = sign y

      local
         val maxShift32 = 0w128
         val maxShift = Word32.toWord maxShift32
         fun make f (arg, shift) =
            let
               fun loop (arg, shift) =
                  if Word.<= (shift, maxShift)
                     then f (arg, Word32.fromWord shift)
                     else loop (f (arg, maxShift32),
                                Word.- (shift, maxShift))
            in
               loop (arg, shift)
            end
      in
         val << = make <<
         val ~>> = make ~>>
      end

      val fromInt = schckFromInt
      val toInt = schckToInt
      val fromLarge = schckFromLargeInt
      val toLarge = schckToLargeInt

      local
         open StringCvt

         val binCvt = mkCvt {base = 2, smallCvt = I.fmt BIN}
         val octCvt = mkCvt {base = 8, smallCvt = I.fmt OCT}
         val decCvt = mkCvt {base = 10, smallCvt = I.fmt DEC}
         val hexCvt = mkCvt {base = 16, smallCvt = I.fmt HEX}
      in
         fun fmt radix =
            case radix of
               BIN => binCvt
             | OCT => octCvt
             | DEC => decCvt
             | HEX => hexCvt
         val toString = fmt DEC
      end

      local
         open StringCvt

         (*
          * Given a char, if it is a digit in the appropriate base,
          * convert it to a word.  Otherwise, return NONE.
          * Note, both a-f and A-F are accepted as hexadecimal digits.
          *)
         fun binDig (ch: char): W.word option =
            case ch of
               #"0" => SOME 0w0
             | #"1" => SOME 0w1
             | _ => NONE

         local
            val op <= = Char.<=
         in
            fun octDig (ch: char): W.word option =
               if #"0" <= ch andalso ch <= #"7"
                  then SOME (W.fromInt (Int.- (Char.ord ch, 
                                               Char.ord #"0")))
               else NONE

            fun decDig (ch: char): W.word option =
               if #"0" <= ch andalso ch <= #"9"
                  then SOME (W.fromInt (Int.- (Char.ord ch, 
                                               Char.ord #"0")))
               else NONE

            fun hexDig (ch: char): W.word option =
               if #"0" <= ch andalso ch <= #"9"
                  then SOME (W.fromInt (Int.- (Char.ord ch, 
                                               Char.ord #"0")))
               else if #"a" <= ch andalso ch <= #"f"
                  then SOME (W.fromInt (Int.- (Char.ord ch, 
                                               Int.- (Char.ord #"a", 0xa))))
               else if #"A" <= ch andalso ch <= #"F"
                  then SOME (W.fromInt (Int.- (Char.ord ch, 
                                               Int.- (Char.ord #"A", 0xA))))
               else NONE
         end

         (*
          * Given a digit converter and a char reader, return a digit
          * reader.
          *)
         fun toDigR (charToDig: char -> W.word option,
                     cread: (char, 'a) reader)
                    (s: 'a)
                    : (W.word * 'a) option =
            case cread s of
               NONE => NONE
             | SOME (ch, s') =>
                  case charToDig ch of
                     NONE => NONE
                   | SOME dig => SOME (dig, s')

         (*
          * A chunk represents the result of processing some digits.
          * more is a bool indicating if there might be more digits.
          * shift is base raised to the number-of-digits-seen power.
          * chunk is the value of the digits seen.
          *)
         type chunk = {more: bool,
                       shift: W.word,
                       chunk: W.word}
         (*
          * Given the base and a digit reader, 
          * return a chunk reader.
          *)
         fun toChunkR (base: W.word,
                       dread: (W.word, 'a) reader)
                      : (chunk, 'a) reader =
            let 
               fun loop {left: Int32.int,
                         shift: W.word,
                         chunk: W.word,
                         s: 'a}
                        : chunk * 'a =
                  if Int32.<= (left, 0)
                     then ({more = true,
                            shift = shift,
                            chunk = chunk},
                           s)
                     else
                        case dread s of
                           NONE => ({more = false,
                                     shift = shift,
                                     chunk = chunk},
                                    s)
                         | SOME (dig, s') =>
                              loop {left = Int32.- (left, 1),
                                    shift = W.* (base, shift),
                                    chunk = W.+ (W.* (base, chunk), dig),
                                    s = s'}
               (* digitsPerChunk = floor((W.wordSize - 3) / (log2 base)) *)
               val digitsPerChunk =
                  case (W.wordSize, base) of
                     (64, 0w16) => 15
                   | (64, 0w10) => 18
                   | (64, 0w8) => 20
                   | (64, 0w2) => 61
                   | (32, 0w16) => 7
                   | (32, 0w10) => 8
                   | (32, 0w8) => 9
                   | (32, 0w2) => 29
                   | _ => raise (Fail "IntInf.scan:digitsPerChunk")
               fun reader (s: 'a): (chunk * 'a) option =
                  case dread s of
                     NONE => NONE
                   | SOME (dig, next) =>
                        SOME (loop {left = Int32.- (digitsPerChunk, 1),
                                    shift = base,
                                    chunk = dig,
                                    s = next})
            in 
               reader
            end

         (*
          * Given a chunk reader, return an unsigned reader.
          *)
         fun toUnsR (ckread: (chunk, 'a) reader): (int, 'a) reader =
            let 
               fun loop (more: bool, acc: int, s: 'a) =
                  if more
                     then case ckread s of
                             NONE => (acc, s)
                           | SOME ({more, shift, chunk}, s') =>
                                loop (more,
                                      ((Prim.addTagCoerce shift) * acc)
                                      + (Prim.addTagCoerce chunk),
                                      s')
                     else (acc, s)
               fun reader (s: 'a): (int * 'a) option =
                  case ckread s of
                     NONE => NONE
                   | SOME ({more, chunk, ...}, s') =>
                        SOME (loop (more,
                                    Prim.addTagCoerce chunk,
                                    s'))
            in 
               reader
            end

         (*
          * Given a char reader and an unsigned reader, return an unsigned
          * reader that includes skipping the option hex '0x'.
          *)
         fun toHexR (cread: (char, 'a) reader, uread: (int, 'a) reader) s =
            case cread s of
               NONE => NONE
             | SOME (c1, s1) =>
                  if c1 = #"0" then
                     case cread s1 of
                        NONE => SOME (zero, s1)
                      | SOME (c2, s2) =>
                           if c2 = #"x" orelse c2 = #"X" then
                              case uread s2 of 
                                 NONE => SOME (zero, s1)
                               | SOME x => SOME x
                              else uread s
                     else uread s

         (*
          * Given a char reader and an unsigned reader, return a signed
          * reader.  This includes skipping any initial white space.
          *)
         fun toSign (cread: (char, 'a) reader, uread: (int, 'a) reader)
                    : (int, 'a) reader =
            let
               fun reader (s: 'a): (int * 'a) option =
                  case cread (StringCvt.skipWS cread s) of
                     NONE => NONE
                   | SOME (ch, s') =>
                       let
                          val (isNeg, s'') =
                             case ch of
                                #"+" => (false, s')
                              | #"-" => (true, s')
                              | #"~" => (true, s')
                              | _ => (false, s)
                       in
                          if isNeg 
                             then case uread s'' of
                                     NONE => NONE
                                   | SOME (abs, s''') => SOME (~ abs, s''')
                             else uread s''
                       end
            in
               reader
            end

         (*
          * Base-specific conversions from char readers to
          * int readers.
          *)
         local
            fun reader (base, dig)
                       (cread: (char, 'a) reader)
                       : (int, 'a) reader =
               let 
                  val dread = toDigR (dig, cread)
                  val ckread = toChunkR (base, dread)
                  val uread = toUnsR ckread
                  val hread = if base = 0w16 then toHexR (cread, uread) else uread
                  val reader = toSign (cread, hread)
               in 
                  reader
               end
         in
            fun binReader z = reader (0w2, binDig) z
            fun octReader z = reader (0w8, octDig) z
            fun decReader z = reader (0w10, decDig) z
            fun hexReader z = reader (0w16, hexDig) z
         end     
      in
         fun scan radix =
            case radix of
               BIN => binReader
             | OCT => octReader
             | DEC => decReader
             | HEX => hexReader
      end

      val fromString = StringCvt.scanString (scan StringCvt.DEC)

      local
         fun isEven (n: Int.int) = Int.andb (n, 0x1) = 0
      in
         fun pow (i: int, j: Int.int): int =
            if Int.< (j, 0) then
               if i = zero then
                  raise Div
               else
                  if i = one then one
                  else if i = negOne then if isEven j then one else negOne
                  else zero
            else
               if j = 0 then one
               else
                  let
                     fun square (n: int): int = n * n
                     (* pow (j) returns (i ^ j) *)
                     fun pow (j: Int.int): int =
                        if Int.<= (j, 0) then one
                        else if isEven j then evenPow j
                        else i * evenPow (Int.- (j, 1))
                     (* evenPow (j) returns (i ^ j), assuming j is even *)
                     and evenPow (j: Int.int): int =
                        square (pow (Int.div (j, 2)))
                  in 
                     pow j
                  end
      end

      val log2 = 
         mkLog2 {fromSmall = fn {smallLog2} => Int32.toInt smallLog2,
                 fromLarge = fn {numLimbsMinusOne, mostSigLimbLog2} =>
                 Int.+ (Int.* (MPLimb.wordSize, SeqIndex.toInt numLimbsMinusOne),
                        Int32.toInt mostSigLimbLog2)}

      val isSmall = Prim.isSmall
      val areSmall = Prim.areSmall
   end
