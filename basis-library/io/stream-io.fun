(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature STREAM_IO_EXTRA_ARG = 
   sig
      structure Array: MONO_ARRAY
      structure ArraySlice: MONO_ARRAY_SLICE
      structure PrimIO: PRIM_IO
      structure Vector: MONO_VECTOR
      structure VectorSlice: MONO_VECTOR_SLICE
      sharing type PrimIO.elem 
         = Vector.elem = VectorSlice.elem
         = Array.elem = ArraySlice.elem 
      sharing type PrimIO.vector 
         = Vector.vector = VectorSlice.vector
         = Array.vector = ArraySlice.vector 
      sharing type PrimIO.vector_slice 
         = VectorSlice.slice
         = ArraySlice.vector_slice
      sharing type PrimIO.array 
         = Array.array = ArraySlice.array 
      sharing type PrimIO.array_slice 
         = ArraySlice.slice

      val line: {isLine: PrimIO.elem -> bool,
                 lineElem: PrimIO.elem} option
      val someElem: PrimIO.elem
      val xlatePos : {toInt : PrimIO.pos -> Position.int,
                      fromInt : Position.int -> PrimIO.pos} option
   end

functor StreamIOExtra (S: STREAM_IO_EXTRA_ARG): STREAM_IO_EXTRA =
   struct
      open S

      structure PIO = PrimIO
      structure A = Array
      structure AS = ArraySlice
      structure V = struct
                      open Vector
                      val extract : vector * int * int option -> vector
                         = VectorSlice.vector o VectorSlice.slice
                    end
      structure VS = VectorSlice

      type elem = PIO.elem
      type vector = PIO.vector
      type vector_slice = PIO.vector_slice
      type reader = PIO.reader
      type writer = PIO.writer
      type pos = PIO.pos

      fun liftExn name function cause = raise IO.Io {name = name,
                                                     function = function,
                                                     cause = cause}

      (*---------------*)
      (*   outstream   *)
      (*---------------*)

      datatype buf = Buf of {array: A.array,
                             size: int ref}
      datatype bufferMode = NO_BUF
                           | LINE_BUF of buf
                           | BLOCK_BUF of buf
      fun newLineBuf bufSize =
        LINE_BUF (Buf {size = ref 0, 
                       array = A.array (bufSize, someElem)})
      fun newBlockBuf bufSize =
        BLOCK_BUF (Buf {size = ref 0, 
                        array = A.array (bufSize, someElem)})

      datatype state = Active | Terminated | Closed
      fun active state = case state of Active => true | _ => false
      fun terminated state = not (active state)
      fun closed state = case state of Closed => true | _ => false

      datatype outstream = Out of {writer: writer,
                                   augmented_writer: writer,
                                   state: state ref,
                                   bufferMode: bufferMode ref}

      fun equalsOut (Out {state = state1, ...}, Out {state = state2, ...}) =
         state1 = state2

      fun outstreamSel (Out v, sel) = sel v
      fun outstreamWriter os = outstreamSel (os, #writer)
      fun writerSel (PIO.WR v, sel) = sel v
      fun outstreamName os = writerSel (outstreamWriter os, #name)

      local
         fun flushGen (write: 'a -> int,
                       base: 'a -> ('b * int * int),
                       slice: ('b * int * int option) -> 'a,
                       a: 'a) =
            let
               val (b, i, sz) = base a
               val max = i + sz
               fun loop i =
                  if i = max
                     then ()
                     else let
                             val j = write (slice (b, i, SOME (max - i)))
                          in 
                             if j = 0
                                then raise (Fail "partial write")
                                else loop (i + j)
                          end
            in
               loop i
            end
      in
         fun flushVec (writer, x) =
            case writerSel (writer, #writeVec) of
               NONE => raise IO.BlockingNotSupported
             | SOME writeVec => flushGen (writeVec, VS.base, VS.slice, x)

         fun flushArr (writer, x) =
            case writerSel (writer, #writeArr) of
               NONE => raise IO.BlockingNotSupported
             | SOME writeArr => flushGen (writeArr, AS.base, AS.slice, x)
      end

      fun flushBuf' (writer, size, array) =
         let
            val size' = !size 
         in 
            size := 0
            ; flushArr (writer, AS.slice (array, 0, SOME size'))
         end

      fun flushBuf (writer, Buf {size, array}) = flushBuf' (writer, size, array)

      fun output (os as Out {augmented_writer,
                             state, 
                             bufferMode, ...}, v) =
         if terminated (!state)
            then liftExn (outstreamName os) "output" IO.ClosedStream
            else let
                    fun put () = flushVec (augmented_writer, VS.full v)
                    fun doit (buf as Buf {size, array}, maybe) =
                       let
                          val curSize = !size
                          val newSize = curSize + V.length v
                       in
                          if newSize >= A.length array orelse maybe ()
                             then (flushBuf (augmented_writer, buf); put ())
                             else (A.copyVec {src = v, dst = array, di = curSize};
                                   size := newSize)
                       end
                 in
                    case !bufferMode of
                       NO_BUF => put ()
                     | LINE_BUF buf => doit (buf, fn () => (case line of
                                                               NONE => false 
                                                             | SOME {isLine, ...} => V.exists isLine v))
                     | BLOCK_BUF buf => doit (buf, fn () => false)
                 end
                 handle exn => liftExn (outstreamName os) "output" exn

      fun ensureActive (os as Out {state, ...}) =
         if active (!state)
            then ()
         else liftExn (outstreamName os) "output" IO.ClosedStream

      local
         val buf1 = A.array (1, someElem)
         fun flush (os, size, array) =
            let
               val Out {augmented_writer, ...} = os
            in
               flushBuf' (augmented_writer, size, array)
               handle exn => liftExn (outstreamName os) "output1" exn
            end
      in
         (* output1 is implemented very carefully to make it fast.  Think hard
          * before modifying it, and test after you do, to make sure that it
          * hasn't been slowed down.
          *)
         fun output1 (os as Out {bufferMode, ...}, c): unit =
            case !bufferMode of
               BLOCK_BUF (Buf {array, size}) =>
                  let
                     val n = !size
                  in
                     (* Use the bounds check for the update to make sure there
                      * is space to put the character in the array.
                      *)
                     (A.update (array, n, c)
                      ; size := 1 + n)
                     handle Subscript =>
                        let
                           val _ = ensureActive os
                           val _ = flush (os, size, array)
                           val _ = A.update (array, 0, c)
                           val _ = size := 1
                        in
                           ()
                        end
                  end
             | LINE_BUF (Buf {array, size}) =>
                  let
                     val n = !size
                     val _ = 
                        (* Use the bounds check for the update to make sure there
                         * is space to put the character in the array.
                         *)
                        (A.update (array, n, c)
                         ; size := 1 + n)
                        handle Subscript =>
                           let
                              val _ = ensureActive os
                              val _ = flush (os, size, array)
                              val _ = A.update (array, 0, c)
                              val _ = size := 1
                           in
                              ()
                           end
                  in
                     case line of
                        NONE => ()
                      | SOME {isLine, ...} =>
                           if isLine c then flush (os, size, array) else ()
                  end
             | NO_BUF =>
                  let
                     val _ = ensureActive os
                     val _ = A.update (buf1, 0, c)
                     val Out {augmented_writer, ...} = os
                  in
                     flushArr (augmented_writer, AS.slice (buf1, 0, SOME 1))
                  end
      end

      fun outputSlice (os as Out {augmented_writer,
                                  state, 
                                  bufferMode, ...}, v) =
         if terminated (!state)
            then liftExn (outstreamName os) "output" IO.ClosedStream
            else let
                    fun put () = flushVec (augmented_writer, v)
                    fun doit (buf as Buf {size, array}, maybe) =
                       let
                          val curSize = !size
                          val newSize = curSize + VS.length v
                       in
                          if newSize >= A.length array orelse maybe ()
                             then (flushBuf (augmented_writer, buf); put ())
                             else (AS.copyVec {src = v, dst = array, di = curSize};
                                   size := newSize)
                       end
                 in
                    case !bufferMode of
                       NO_BUF => put ()
                     | LINE_BUF buf => doit (buf, fn () => (case line of
                                                               NONE => false 
                                                             | SOME {isLine, ...} => VS.exists isLine v))
                     | BLOCK_BUF buf => doit (buf, fn () => false)
                 end
                 handle exn => liftExn (outstreamName os) "output" exn

      fun flushOut (os as Out {augmented_writer, 
                               state, 
                               bufferMode, ...}) =
        if terminated (!state)
          then ()
          else case !bufferMode of
                 NO_BUF => ()
               | LINE_BUF buf => flushBuf (augmented_writer, buf)
               | BLOCK_BUF buf => flushBuf (augmented_writer, buf)
        handle exn => liftExn (outstreamName os) "flushOut" exn

      fun makeTerminated (Out {bufferMode, ...}) =
         let
            fun doit (Buf {array, size}) = size := A.length array
         in
            case !bufferMode of
               BLOCK_BUF b => doit b
             | LINE_BUF b => doit b
             | NO_BUF => ()
         end

      fun closeOut (os as Out {state, ...}) =
        if closed (!state)
          then ()
          else (flushOut os;
                if terminated (!state)
                  then ()
                  else (writerSel (outstreamWriter os, #close)) ();
                state := Closed
                ; makeTerminated os)
        handle exn => liftExn (outstreamName os) "closeOut" exn

      fun getBufferMode (Out {bufferMode, ...}) =
         case !bufferMode of
            NO_BUF => IO.NO_BUF
          | LINE_BUF _ => IO.LINE_BUF
          | BLOCK_BUF _ => IO.BLOCK_BUF

      fun setBufferMode (os as Out {bufferMode, ...}, mode) =
        case mode of
          IO.NO_BUF => (flushOut os;
                        bufferMode := NO_BUF)
        | IO.LINE_BUF => let
                           fun doit () = 
                             bufferMode := 
                             newLineBuf (writerSel (outstreamWriter os, #chunkSize))
                         in
                           case !bufferMode of
                             NO_BUF => doit ()
                           | LINE_BUF _ => ()
                           | BLOCK_BUF _ => doit ()
                         end
        | IO.BLOCK_BUF => let
                            fun doit () = 
                              bufferMode := 
                              newBlockBuf (writerSel (outstreamWriter os, #chunkSize))
                          in
                            case !bufferMode of
                              NO_BUF => doit ()
                            | LINE_BUF _ => doit ()
                            | BLOCK_BUF _ => ()
                          end

      fun mkOutstream' {writer, closed, bufferMode} =
        let
          val bufSize = writerSel (writer, #chunkSize)
        in
          Out {writer = writer,
               augmented_writer = PIO.augmentWriter writer,
               state = ref (if closed then Closed else Active),
               bufferMode = ref (case bufferMode of
                                    IO.NO_BUF => NO_BUF
                                  | IO.LINE_BUF => newLineBuf bufSize
                                  | IO.BLOCK_BUF => newBlockBuf bufSize)}
        end
      fun mkOutstream (writer, bufferMode) =
        mkOutstream' {writer = writer, closed = false, bufferMode = bufferMode}

      fun getWriter (os as Out {writer, state, bufferMode, ...}) =
        if closed (!state)
          then liftExn (outstreamName os) "getWriter" IO.ClosedStream
          else (flushOut os
                ; state := Terminated
                ; makeTerminated os
                ; (writer,
                   case !bufferMode of
                      NO_BUF => IO.NO_BUF
                    | LINE_BUF _ => IO.LINE_BUF
                    | BLOCK_BUF _ => IO.BLOCK_BUF))

      datatype out_pos = OutPos of {pos: pos,
                                    outstream: outstream}

      fun getPosOut (os as Out {...}) =
        (flushOut os;
         case writerSel (outstreamSel (os, #writer), #getPos) of
           NONE => liftExn (outstreamName os) "getPosOut" IO.RandomAccessNotSupported
         | SOME getPos => OutPos {pos = getPos (),
                                  outstream = os})

      fun setPosOut (OutPos {pos, outstream = os}) =
        (flushOut os;
         case writerSel (outstreamSel (os, #writer), #setPos) of
           NONE => liftExn (outstreamName os) "setPosOut" IO.RandomAccessNotSupported
         | SOME setPos => setPos pos;
         os)

      fun filePosOut (OutPos {pos, ...}) = pos

      (*---------------*)
      (*   instream    *)
      (*---------------*)

      datatype state = Link of {buf: buf}
                     | Eos of {buf: buf} (* V.length inp = 0 *)
                     | End
                     | Truncated
                     | Closed
      and buf = Buf of {inp: V.vector,
                        base: pos option,
                        next: state ref}

      datatype instream = In of {common: {reader: reader,
                                          augmented_reader: reader,
                                          tail: state ref ref},
                                 pos: int,
                                 buf: buf}
      (* @ s = Eos, End, Truncated, Closed ==>
       *   pos = V.length inp, !next = s
       *)

      fun equalsIn (In {common = {tail = tail1, ...}, ...}, 
                    In {common = {tail = tail2, ...}, ...}) = 
        tail1 = tail2

      fun update (In {common, ...}, pos, buf) =
        In {common = common,
            pos = pos,
            buf = buf}
      fun updatePos (is as In {buf, ...}, pos) = update (is, pos, buf)
      fun updateBufBeg (is, buf) = update (is, 0, buf)
      fun updateBufEnd (is, buf as Buf {inp, ...}) = update (is, V.length inp, buf)

      fun instreamSel (In v, sel) = sel v
      fun instreamCommon is = instreamSel (is, #common)
      fun instreamCommonSel (is, sel) = sel (instreamCommon is)
      fun instreamReader is = instreamCommonSel (is, #reader)
      fun instreamTail is = instreamCommonSel (is, #tail)
      fun readerSel (PIO.RD v, sel) = sel v
      fun instreamName is = readerSel (instreamReader is, #name)

      val empty = V.tabulate (0, fn _ => someElem)

      fun extend function
                 (is as In {common = {augmented_reader, tail, ...}, ...})
                 blocking =
        case !(!tail) of
          End =>
            let
              fun link (base, inp) = let
                                       val next = ref End
                                       val buf = Buf {inp = inp,
                                                      base = base,
                                                      next = next}
                                       val this = if V.length inp = 0
                                                    then Eos {buf = buf}
                                                    else Link {buf = buf}
                                       val _ = !tail := this
                                       val _ = tail := next
                                     in
                                       SOME this
                                     end
              fun doit readVec =
                let
                  val base =
                    case readerSel (augmented_reader, #getPos) of
                      NONE => NONE
                    | SOME getPos => SOME (getPos ())
                  val inp = readVec (readerSel (augmented_reader, #chunkSize))
                            handle exn =>
                            liftExn (instreamName is) function exn
                in
                  case inp of
                    NONE => NONE
                  | SOME inp => link (base, inp)
                end
            in
              if blocking 
                then case readerSel (augmented_reader, #readVec) of
                       NONE => liftExn (instreamName is) 
                                       function 
                                       IO.BlockingNotSupported
                     | SOME readVec => doit (SOME o readVec)
                else case readerSel (augmented_reader, #readVecNB) of
                       NONE => liftExn (instreamName is) 
                                       function 
                                       IO.NonblockingNotSupported
                     | SOME readVecNB => doit readVecNB
            end
        | _ => liftExn (instreamName is) function Match

      fun extendB function is = valOf (extend function is true)
      fun extendNB function is = extend function is false

      fun input (is as In {pos, buf as Buf {inp, next, ...}, ...}) =
        if pos < V.length inp
          then (V.extract(inp, pos, NONE), 
                updateBufEnd (is, buf))
          else let
                 fun doit next =
                   case next of
                     Link {buf as Buf {inp, ...}} => (inp, updateBufEnd (is, buf))
                   | Eos {buf} => (empty, updateBufBeg (is, buf))
                   | End => doit (extendB "input" is)
                   | _ => (empty, is)
               in
                 doit (!next)
               end

      fun inputN (is, n) =
        if n < 0 orelse n > V.maxLen
          then raise Size
          else let
                 fun first (is as In {pos, buf as Buf {inp, ...}, ...}, n) =
                   if pos + n <= V.length inp
                     then let
                            val inp' = V.extract(inp, pos, SOME n)
                          in
                            (inp', updatePos (is, pos + n))
                          end
                     else let
                            val inp' = VS.slice(inp, pos, NONE)
                          in
                            loop (buf, [inp'], n - (V.length inp - pos))
                          end
                 and loop (buf' as Buf {next, ...}, inps, n) =
                   let
                     fun doit next =
                       case next of
                         Link {buf as Buf {inp, ...}} =>
                           if n <= V.length inp
                             then let
                                    val inp' = VS.slice(inp, 0, SOME n)
                                    val inps = inp'::inps
                                  in
                                    finish (inps, update (is, n, buf))
                                  end
                             else loop (buf, (VS.full inp)::inps, n - V.length inp)
                       | Eos {buf} => 
                           finish (inps, if n > 0
                                           then updateBufBeg (is, buf)
                                           else updateBufEnd (is, buf'))
                       | End => doit (extendB "inputN" is)
                       | _ => finish (inps, updateBufEnd (is, buf'))
                   in
                     doit (!next)
                   end
                 and finish (inps, is) =
                   let val inp = VS.concat (List.rev inps)
                   in (inp, is)
                   end
               in
                 first (is, n)
               end

      (* input1' will move past a temporary end of stream *)
      fun input1' (is as In {pos, buf = Buf {inp, next, ...}, ...}) =
         case SOME (V.sub (inp, pos)) handle Subscript => NONE of
            NONE =>
               let
                  fun doit next =
                     case next of
                        Link {buf} => input1' (updateBufBeg (is, buf))
                      | Eos {buf} => (NONE, updateBufBeg (is, buf))
                      | End => doit (extendB "input1" is)
                      | _ => (NONE, is)
               in
                  doit (!next)
               end
          | SOME e =>
               let
                  val is' = updatePos (is, pos + 1)
               in
                  (SOME e, is')
               end

      (* input1 will never move past a temporary end of stream *)
      fun input1 is =
        case input1' is of
          (SOME c, is') => SOME (c, is')
        | _ => NONE

      fun inputAll is =
        let
          fun loop (is, ac) =
            let val (inp, is') = input is
            in
              if V.length inp = 0
                then (V.concat (List.rev ac), is')
                else loop (is', inp::ac)
            end
        in
          loop (is, [])
        end

      val inputLine =
         case line of
            NONE => (fn is => SOME (input is))
          | SOME {isLine, lineElem, ...} =>
            let
               val lineVecSl = VS.full (V.tabulate(1, fn _ => lineElem))
            in
               fn is =>
               let
                  fun findLine (v, i) =
                     let
                        fun loop i =
                           case SOME (V.sub (v, i)) handle Subscript => NONE of
                              NONE => NONE
                            | SOME c => 
                                 if isLine c
                                    then SOME (i + 1)
                                 else loop (i + 1)
                     in
                        loop i
                     end
                  fun first (is as In {pos, buf as Buf {inp, next, ...}, ...}) =
                     (case findLine (inp, pos) of
                         SOME i => let
                                      val inp' = V.extract(inp, pos, SOME (i - pos))
                                   in
                                      SOME (inp', updatePos (is, i))
                                   end
                       | NONE => if pos < V.length inp
                                    then let
                                            val inp' = VS.slice(inp, pos, NONE)
                                         in
                                            loop (buf, [inp'])
                                         end
                                    else let
                                            fun doit next = 
                                               case next of
                                                  Link {buf} => first (updateBufBeg (is, buf))
                                                | Eos _ => NONE
                                                | End => doit (extendB "inputLine" is)
                                                | _ => NONE
                                         in
                                            doit (!next)
                                         end)
                  and loop (buf' as Buf {next, ...}, inps) = 
                     (* List.length inps > 0 *)
                     let
                        fun doit next =
                           case next of
                              Link {buf as Buf {inp, ...}} =>
                                 (case findLine (inp, 0) of
                                     SOME i => let
                                                  val inp' = VS.slice(inp, 0, SOME i)
                                                  val inps = inp'::inps
                                               in
                                                  finish (inps, update (is, i, buf), false)
                                               end
                                   | NONE => loop (buf, (VS.full inp)::inps))
                            | End => doit (extendB "inputLine" is)
                            | _ => finish (inps, updateBufEnd (is, buf'), true)
                     in
                        doit (!next)
                     end
                  and finish (inps, is, trail) =
                     let
                        val inps = if trail
                                      then lineVecSl::inps
                                      else inps
                        val inp = VS.concat (List.rev inps)
                     in
                        SOME (inp, is)
                     end
               in
                  first is
               end
            end

      fun canInput (is as In {pos, buf = Buf {inp, next, ...}, ...}, n) =
        if n < 0 orelse n > V.maxLen
          then raise Size
        else if n = 0
          then SOME 0
        else let
               fun start inp = 
                 add ([], inp, 0)
               and add (inps, inp, k) =
                 let
                   val l = V.length inp
                   val inps = inp::inps
                 in
                   if k + l > n
                     then finish (inps, n)
                     else loop (inps, k + l)
                 end
               and loop (inps, k) =
                 case extendNB "canInput" is of
                   NONE => finish (inps, k)
                 | SOME (Link {buf = Buf {inp, ...}}) =>
                     add (inps, inp, k)
                 | SOME (Eos _) => finish (inps, k)
                 | _ => raise Fail "extendNB bug"
               and finish (inps, k) =
                 let
                   val inp = V.concat (List.rev inps)
                 in
                   (inp, k)
                 end
             in
               if pos < V.length inp
                 then SOME (Int.min (V.length inp - pos, n))
                 else case !next of
                        End => 
                          (case extendNB "canInput" is of
                             NONE => NONE
                           | SOME (Link {buf = Buf {inp, base, ...}}) =>
                               let
                                 val (inp, k) = start inp
                                 val buf = Buf {inp = inp,
                                                base = base,
                                                next = ref End}
                               in
                                 next := Link {buf = buf};
                                 SOME k
                               end
                           | SOME (Eos _) => SOME 0
                           | _ => raise Fail "extendNB bug")
                      | _ => SOME 0
             end

      structure Close =
         struct
            datatype t = T of {close: unit -> unit,
                               name: string,
                               tail: state ref ref}

            fun close (T {close, name, tail}) =
               case !(!tail) of
                  End =>
                     (!tail := Closed
                      ; close () handle exn => liftExn name "closeIn" exn)
                | _ => ()

            fun equalsInstream (T {tail, ...}, is) = tail = instreamTail is

            fun make (In {common = {reader = PIO.RD {close, name, ...},
                                    tail, ...},
                          ...}): t =
               T {close = close, name = name, tail = tail}
         end

      val closeIn = Close.close o Close.make

      fun endOfStream is =
        let val (inp, _) = input is
        in V.length inp = 0
        end

      fun mkInstream' {bufferContents, closed, reader} =
        let
          val next = ref (if closed then Closed else End)
          val base =
            case readerSel (reader, #getPos) of
              NONE => NONE
            | SOME getPos => SOME (getPos ())
          val buf = 
            case bufferContents of
              NONE => Buf {inp = empty,
                           base = base,
                           next = next}
            | SOME (lastRead, v) => 
                if V.length v = 0
                  then Buf {inp = empty,
                            base = base,
                            next = ref (Eos {buf = Buf {inp = empty,
                                                        base = base,
                                                        next = next}})}
                  else case (lastRead, base, xlatePos) of
                         (true, SOME b, SOME {fromInt, toInt, ...}) =>
                           let
                             val b =
                               fromInt (Position.- (toInt b, Position.fromInt (V.length v)))
                           in
                             Buf {inp = v,
                                  base = SOME b,
                                  next = next}
                           end
                       | _ => Buf {inp = v,
                                   base = NONE,
                                   next = next}
        in
          In {common = {reader = reader,
                        augmented_reader = PIO.augmentReader reader,
                        tail = ref next},
              pos = 0,
              buf = buf}
        end

      fun mkInstream (reader, bufferContents) =
        mkInstream' {bufferContents = if 0 = V.length bufferContents
                                         then NONE
                                         else SOME (false, bufferContents),
                     closed = false, 
                     reader = reader}

      fun getReader (is as In {common = {reader, tail, ...}, ...}) =
        case !(!tail) of
          End => (!tail := Truncated;
                  let val (inp, _) = inputAll is
                  in (reader, inp)
                  end)
        | _ => liftExn (instreamName is) "getReader" IO.ClosedStream

      fun filePosIn (is as In {common = {augmented_reader, ...},
                               pos,
                               buf = Buf {base, ...}, ...}) =
        case base of
           SOME b => (case xlatePos of
                         SOME {fromInt, toInt, ...} => 
                            (fromInt (Position.+ (Position.fromInt pos, toInt b)))
                       | NONE => (case (readerSel (augmented_reader, #readVec),
                                        readerSel (augmented_reader, #getPos),
                                        readerSel (augmented_reader, #setPos)) of
                                     (SOME readVec, SOME getPos, SOME setPos) => 
                                        let
                                           val curPos = getPos ()
                                        in
                                           setPos b
                                           ; ignore (readVec pos)
                                           ; getPos () before setPos curPos
                                        end
                                   | _ => 
                                        liftExn (instreamName is) "filePosIn" IO.RandomAccessNotSupported))
         | NONE => liftExn (instreamName is) "filePosIn" IO.RandomAccessNotSupported
   end

signature STREAM_IO_ARG = 
   sig 
      structure Array: MONO_ARRAY
      structure ArraySlice: MONO_ARRAY_SLICE
      structure PrimIO: PRIM_IO  
      structure Vector: MONO_VECTOR
      structure VectorSlice: MONO_VECTOR_SLICE
      sharing type PrimIO.elem = Vector.elem = VectorSlice.elem = Array.elem
         = ArraySlice.elem 
      sharing type PrimIO.vector = Vector.vector = VectorSlice.vector
         = Array.vector = ArraySlice.vector 
      sharing type PrimIO.vector_slice = VectorSlice.slice
         = ArraySlice.vector_slice
      sharing type PrimIO.array = Array.array = ArraySlice.array 
      sharing type PrimIO.array_slice = ArraySlice.slice

      val someElem: PrimIO.elem
   end

functor StreamIO (S: STREAM_IO_ARG): STREAM_IO =
   StreamIOExtra (open S
                  val line = NONE
                  val xlatePos = NONE)

signature STREAM_IO_EXTRA_FILE_ARG = STREAM_IO_EXTRA_ARG

functor StreamIOExtraFile (S: STREAM_IO_EXTRA_FILE_ARG): STREAM_IO_EXTRA_FILE =
   struct
      open S

      structure PIO = PrimIO
      structure V = Vector

      structure StreamIO = StreamIOExtra (S)
      open StreamIO

      fun liftExn name function cause = raise IO.Io {name = name,
                                                     function = function,
                                                     cause = cause}

      (*---------------*)
      (*   outstream   *)
      (*---------------*)

      fun writerSel (PIO.WR v, sel) = sel v
      fun outstreamName os = writerSel (outstreamWriter os, #name)

      fun outFd os =
        case writerSel (outstreamWriter os, #ioDesc) of
          SOME ioDesc => valOf (Posix.FileSys.iodToFD ioDesc)
        | NONE => liftExn (outstreamName os) "outFd" (Fail "<no ioDesc>")

      val openOutstreams : (outstream * {close: bool}) list ref = ref []

      val mkOutstream'' =
         let    
            val _ = Cleaner.addNew
               (Cleaner.atExit, fn () =>
                List.app (fn (os, {close}) =>
                          if close
                             then closeOut os
                          else flushOut os) (!openOutstreams))
         in
            fn {bufferMode, closeAtExit, closed, writer} =>
            let
               val os = mkOutstream' {bufferMode = bufferMode,
                                      closed = closed,
                                      writer = writer}
               val _ =
                  if closed
                     then ()
                  else openOutstreams := ((os, {close = closeAtExit})
                                          :: (!openOutstreams))
            in
               os
            end
         end

      fun mkOutstream' {bufferMode, closed, writer} =
         mkOutstream'' {bufferMode = bufferMode,
                        closeAtExit = true,
                        closed = closed, 
                        writer = writer}

      fun mkOutstream (writer, bufferMode) =
        mkOutstream' {bufferMode = bufferMode,
                      closed = false,
                      writer = writer}

      val closeOut = fn os =>
        let
          val _ = openOutstreams := List.filter (fn (os', _) => 
                                                 not (equalsOut (os, os'))) 
                                                (!openOutstreams)
        in
          closeOut os
        end

      (*---------------*)
      (*   instream    *)
      (*---------------*)

      fun readerSel (PIO.RD v, sel) = sel v

      fun instreamName is = readerSel (instreamReader is, #name)

      fun inFd is =
        case readerSel (instreamReader is, #ioDesc) of
          SOME ioDesc => valOf (Posix.FileSys.iodToFD ioDesc)
        | NONE => liftExn (instreamName is) "inFd" (Fail "<no ioDesc>")

      val closeAtExits: Close.t list ref = ref []

      val mkInstream'' =
        let
           val _ = Cleaner.addNew (Cleaner.atExit, fn () =>
                                   List.app Close.close (!closeAtExits))
        in
          fn {bufferContents, closeAtExit, closed, reader} =>
          let
            val is =
               mkInstream' {bufferContents = bufferContents,
                            closed = closed,
                            reader = reader}
            val _ =
               if closed orelse not closeAtExit
                  then ()
               else closeAtExits := Close.make is :: (!closeAtExits)
          in
            is
          end
        end

      fun mkInstream' {bufferContents, closed, reader} =
        mkInstream'' {bufferContents = bufferContents,
                      closeAtExit = true,
                      closed = closed, 
                      reader = reader}


      fun mkInstream (reader, bufferContents) =
        mkInstream' {bufferContents = (if V.length bufferContents = 0 then NONE
                                       else SOME (false, bufferContents)),
                     closed = false,
                     reader = reader}

      val closeIn = fn is =>
         let
            val _ =
               closeAtExits :=
               List.filter (fn c => Close.equalsInstream (c, is)) (!closeAtExits)
         in
            closeIn is
         end
   end
