(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PRIM_IO_ARG =
   sig
      structure Vector: MONO_VECTOR
      structure VectorSlice: MONO_VECTOR_SLICE
      structure Array: MONO_ARRAY
      structure ArraySlice: MONO_ARRAY_SLICE
      sharing type Vector.elem = VectorSlice.elem 
         = Array.elem = ArraySlice.elem 
      sharing type Vector.vector = VectorSlice.vector 
         = Array.vector = ArraySlice.vector 
      sharing type VectorSlice.slice = ArraySlice.vector_slice
      sharing type Array.array = ArraySlice.array

      val someElem: Vector.elem

      eqtype pos
      val compare: pos * pos -> order
   end

functor PrimIO (S: PRIM_IO_ARG): PRIM_IO =
   struct 
      open S

      structure V = Vector
      structure VS = VectorSlice
      structure A = Array
      structure AS = ArraySlice

      type elem = A.elem
      type vector = V.vector
      type vector_slice = VS.slice
      type array = A.array
      type array_slice = AS.slice
      type pos = pos
      val compare = compare

      datatype reader =
         RD of {avail: unit -> int option,
                block: (unit -> unit) option,
                canInput: (unit -> bool) option,
                chunkSize: int,
                close: unit -> unit,
                endPos: (unit -> pos) option,
                getPos: (unit -> pos) option,
                ioDesc: OS.IO.iodesc option,
                name: string,
                readArr: (array_slice -> int) option,
                readArrNB: (array_slice -> int option) option,
                readVec: (int -> vector) option,
                readVecNB: (int -> vector option) option,
                setPos: (pos -> unit) option,
                verifyPos: (unit -> pos) option}

      datatype writer =
         WR of {block: (unit -> unit) option,
                canOutput: (unit -> bool) option,
                chunkSize: int,
                close: unit -> unit,
                endPos: (unit -> pos) option,
                getPos: (unit -> pos) option,
                ioDesc: OS.IO.iodesc option,
                name: string,
                setPos: (pos -> unit) option,
                verifyPos: (unit -> pos) option,
                writeArr: (array_slice -> int) option,
                writeArrNB: (array_slice -> int option) option,
                writeVec: (vector_slice -> int) option,
                writeVecNB: (vector_slice -> int option) option}


      fun liftExn name function cause = raise IO.Io {name = name,
                                                     function = function,
                                                     cause = cause}

      fun openVector v =
         let
            val name = "openVector"
            val closed = ref false
            val pos = ref 0
            val eofPos = V.length v
            fun check f = if !closed
                             then liftExn name f IO.ClosedStream
                             else ()
            fun const f c = fn _ => (check f; c)
            fun readVec f i =
               let
                  val _ = check f
                  val n = Int.min (i, eofPos - !pos)
               in
                  VS.vector (VS.slice (v, !pos, SOME n)) before (pos := !pos + n)
               end
            fun readArr f sl =
               let
                  val _ = check f
                  val (buf, i, sz) = AS.base sl
                  val n = Int.min (sz, eofPos - !pos)
               in
                  AS.copyVec {src = VS.slice (v, !pos, SOME n),
                              dst = buf,
                              di = i};
                  pos := !pos + n;
                  n
               end
         in
            RD {avail = const "avail" NONE,
                block = SOME (const "block" ()),
                canInput = SOME (const "canInput" true),
                chunkSize = 32,
                close = fn () => (closed := true),
                endPos = NONE,
                getPos = NONE,
                ioDesc = NONE,
                name = name,
                readArr = SOME (readArr "readArr"),
                readArrNB = SOME (SOME o (readArr "readVecNB")),
                readVec = SOME (readVec "readVec"),
                readVecNB = SOME (SOME o (readVec "readVecNB")),
                setPos = NONE,
                verifyPos = NONE}
         end

      fun nullRd () =
         let
            val name = "nullRd"
            val closed = ref false
            fun check f = if !closed
                             then liftExn name f IO.ClosedStream
                             else ()
            fun const f c = fn _ => (check f; c)
            val empty = V.fromList []
         in
            RD {avail = const "avail" NONE,
                block = SOME (const "block" ()),
                canInput = SOME (const "canInput" true),
                chunkSize = 1,
                close = fn () => (closed := true),
                endPos = NONE,
                getPos = NONE,
                ioDesc = NONE,
                name = name,
                readArr = SOME (const "readArr" 0),
                readArrNB = SOME (const "readArrNB" (SOME 0)),
                readVec = SOME (const "readVec" empty),
                readVecNB = SOME (const "readVecNB" (SOME empty)),
                setPos = NONE,
                verifyPos = NONE}
         end

      fun nullWr () =
         let
            val name = "nullWr"
            val closed = ref false
            fun check f = if !closed
                             then liftExn name f IO.ClosedStream
                             else ()
            fun const f c = fn _ => (check f; c)
            fun function f g = fn x => (check f; g x)
         in
            WR {block = SOME (const "block" ()),
                canOutput = SOME (const "canOutput" true),
                chunkSize = 1,
                close = fn () => (closed := true),
                endPos = NONE,
                getPos = NONE,
                ioDesc = NONE,
                name = name,
                setPos = NONE,
                verifyPos = NONE,
                writeArr = SOME (function "writeArr" AS.length),
                writeArrNB = SOME (function "writeArrNB" (SOME o AS.length)),
                writeVec = SOME (function "writeVec" VS.length),
                writeVecNB = SOME (function "writeVecNB" (SOME o VS.length))}
         end

      fun doBlock (f, block: unit -> unit) x = (block (); valOf (f x))
      fun doCanInput (f, canInput) x = if canInput ()
                                          then SOME (f x)
                                          else NONE

      fun augmentReader (RD {name, chunkSize,
                             readVec, readArr, readVecNB, readArrNB,
                             block, canInput, avail,
                             getPos, setPos, endPos, verifyPos,
                             close, ioDesc}) =
         let
            fun augmentRead (readVec, readArr) =
               case (readVec, readArr) of
                  (SOME readVec, SOME readArr) => (SOME readVec, SOME readArr)
                | (NONE, SOME readArr) =>
                     (SOME (fn i =>
                            let
                               val buf = A.array (i, someElem)
                               fun first j = AS.slice (buf, 0, SOME j)
                            in
                               (AS.vector o first) (readArr (first i))
                            end),
                      SOME readArr)
                | (SOME readVec, NONE) =>
                     (SOME readVec,
                      SOME (fn sl =>
                            let
                               val (buf, i, sz) = AS.base sl
                               val v = readVec sz
                               val _ = A.copyVec {src = v, dst = buf, di = i}
                            in
                               V.length v
                            end))
                | (NONE, NONE) => (NONE, NONE)
            fun augmentReadNB (readVecNB, readArrNB) =
               case (readVecNB, readArrNB) of
                  (SOME readVecNB, SOME readArrNB) => (SOME readVecNB, SOME readArrNB)
                | (NONE, SOME readArrNB) => 
                     (SOME (fn i =>
                            let
                               val buf = A.array (i, someElem)
                               fun first j = AS.slice (buf, 0, SOME j)
                            in
                               Option.map (AS.vector o first) (readArrNB (first i))
                            end),
                      SOME readArrNB)
                | (SOME readVecNB, NONE) =>
                     (SOME readVecNB,
                      SOME (fn sl =>
                            let
                               val (buf, i, sz) = AS.base sl
                            in
                               case readVecNB sz of
                                  NONE => NONE
                                | SOME v => (A.copyVec {src = v, dst = buf, di = i}
                                             ; SOME (V.length v))
                            end))
                | (NONE, NONE) => (NONE, NONE)
            fun augmentSeq (readSeq, readSeqNB) =
               case (readSeq, readSeqNB) of
                  (SOME readSeq, SOME readSeqNB) => (SOME readSeq, SOME readSeqNB)
                | (NONE, SOME readSeqNB) =>
                     (case block of
                         NONE => NONE
                       | SOME block => SOME (doBlock (readSeqNB, block)),
                            SOME readSeqNB)
                | (SOME readSeq, NONE) =>
                         (SOME readSeq,
                          case canInput of
                             NONE => NONE
                           | SOME canInput => SOME (doCanInput (readSeq, canInput)))
                | (NONE, NONE) => (NONE, NONE)

            val ((readVec,readArr),(readVecNB,readArrNB)) =
               (augmentRead (readVec, readArr),
                augmentReadNB (readVecNB, readArrNB))
            val ((readVec,readVecNB),(readArr,readArrNB)) =
               (augmentSeq (readVec, readVecNB),
                augmentSeq (readArr, readArrNB))
         in
            RD {name = name, chunkSize = chunkSize,
                readVec = readVec, readArr = readArr, 
                readVecNB = readVecNB, readArrNB = readArrNB,
                block = block, canInput = canInput, avail = avail,
                getPos = getPos, setPos = setPos, 
                endPos = endPos, verifyPos = verifyPos,
                close = close, ioDesc = ioDesc}
         end

      fun augmentWriter (WR {name, chunkSize,
                             writeVec, writeArr, writeVecNB, writeArrNB,
                             block, canOutput,
                             getPos, setPos, endPos, verifyPos,
                             close, ioDesc}) =
         let
            fun augmentWrite (writeVec, writeArr) =
               case (writeVec, writeArr) of
                  (SOME writeVec, SOME writeArr) => (SOME writeVec, SOME writeArr)
                | (NONE, SOME writeArr) =>
                     (SOME (fn sl => 
                            writeArr
                            (AS.full
                             (A.tabulate (VS.length sl, fn i => VS.sub (sl, i))))),
                      SOME writeArr)
                | (SOME writeVec, NONE) =>
                     (SOME writeVec,
                      SOME (fn sl => writeVec (VS.full (AS.vector sl))))
                | (NONE, NONE) => (NONE, NONE)
            fun augmentSeq (writeSeq, writeSeqNB) =
               case (writeSeq, writeSeqNB) of
                  (SOME writeSeq, SOME writeSeqNB) => (SOME writeSeq, SOME writeSeqNB)
                | (NONE, SOME writeSeqNB) =>
                     (case block of
                         NONE => NONE
                       | SOME block => SOME (fn x => (block ();
                                                      valOf (writeSeqNB x))),
                            SOME writeSeqNB)
                | (SOME writeSeq, NONE) =>
                         (SOME writeSeq,
                          case canOutput of
                             NONE => NONE
                           | SOME canOutput => SOME (fn x => (if canOutput ()
                                                                 then SOME (writeSeq x)
                                                                 else NONE)))
                | (NONE, NONE) => (NONE, NONE)

            val ((writeVec,writeArr),(writeVecNB,writeArrNB)) =
               (augmentWrite (writeVec, writeArr),
                augmentWrite (writeVecNB, writeArrNB))
            val ((writeVec,writeVecNB),(writeArr,writeArrNB)) =
               (augmentSeq (writeVec, writeVecNB),
                augmentSeq (writeArr, writeArrNB))
         in
            WR {name = name, chunkSize = chunkSize,
                writeVec = writeVec, writeArr = writeArr, 
                writeVecNB = writeVecNB, writeArrNB = writeArrNB,
                block = block, canOutput = canOutput,
                getPos = getPos, setPos = setPos, 
                endPos = endPos, verifyPos = verifyPos,
                close = close, ioDesc = ioDesc}
         end
   end
