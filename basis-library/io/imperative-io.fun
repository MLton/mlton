(* Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature IMPERATIVE_IO_EXTRA_ARG =
   sig
      structure Array: sig
                          include MONO_ARRAY
                          val arrayUninit: int -> array
                          val unsafeSub: array * int -> elem
                       end
      structure ArraySlice: MONO_ARRAY_SLICE
      structure PrimIO: PRIM_IO
      structure Vector: sig 
                           include MONO_VECTOR
                           val unsafeFromArray: Array.array -> vector
                        end
      structure VectorSlice: MONO_VECTOR_SLICE
      sharing type Array.array
         = ArraySlice.array
         = PrimIO.array
      sharing type Array.elem
         = ArraySlice.elem
         = PrimIO.elem
         = Vector.elem
         = VectorSlice.elem
      sharing type Array.vector
         = ArraySlice.vector
         = PrimIO.vector
         = Vector.vector
         = VectorSlice.vector
      sharing type ArraySlice.slice
         = PrimIO.array_slice
      sharing type ArraySlice.vector_slice
         = PrimIO.vector_slice
         = VectorSlice.slice

      val chunkSize: int
      val fileTypeFlags: Posix.FileSys.O.flags list
      val line : {isLine: Vector.elem -> bool,
                  lineElem: Vector.elem} option
      val mkReader: {fd: Posix.FileSys.file_desc,
                     name: string,
                     initBlkMode: bool} -> PrimIO.reader
      val mkWriter: {fd: Posix.FileSys.file_desc,
                     name: string,
                     appendMode: bool,
                     initBlkMode: bool,
                     chunkSize: int} -> PrimIO.writer
      val someElem: PrimIO.elem
      val xlatePos : {toInt : PrimIO.pos -> Position.int, 
                      fromInt : Position.int -> PrimIO.pos} option
   end

functor ImperativeIOExtra (S: IMPERATIVE_IO_EXTRA_ARG): IMPERATIVE_IO_EXTRA =
struct

open S

structure StreamIO = StreamIOExtraFile (S)

structure PIO = PrimIO
structure SIO = StreamIO
structure A = Array
structure AS = ArraySlice
structure V = Vector
structure VS = VectorSlice

type elem = PrimIO.elem
type vector = PrimIO.vector
type vector_slice = VS.slice

(* ------------------------------------------------- *)
(*                     outstream                     *)
(* ------------------------------------------------- *)

(* The following :> hides the fact that Outstream.t is an eqtype.  Doing it
 * here is much easier than putting :> on the functor result.
 *)
structure Outstream:>
   sig
      type t

      val get: t -> SIO.outstream
      val make: SIO.outstream -> t
      val set: t *  SIO.outstream -> unit
   end =
   struct 
      datatype t = T of SIO.outstream ref

      fun get (T r) = !r
      fun set (T r, s) = r := s
      fun make s = T (ref s)
   end

type outstream = Outstream.t
fun output (os, v) = SIO.output (Outstream.get os, v)
fun output1 (os, v) = SIO.output1 (Outstream.get os, v)
fun outputSlice (os, v) = SIO.outputSlice (Outstream.get os, v)
fun flushOut os = SIO.flushOut (Outstream.get os)
fun closeOut os = SIO.closeOut (Outstream.get os)
val mkOutstream = Outstream.make
val getOutstream = Outstream.get
val setOutstream  = Outstream.set
val getPosOut = SIO.getPosOut o Outstream.get
fun setPosOut (os, outPos) = Outstream.set (os, SIO.setPosOut outPos)

fun newOut {appendMode, bufferMode, closeAtExit, fd, name} =
   let
      val writer = mkWriter {appendMode = appendMode, 
                             chunkSize = chunkSize,
                             fd = fd,
                             initBlkMode = true,
                             name = name}
      val outstream = SIO.mkOutstream'' {bufferMode = bufferMode,
                                         closeAtExit = closeAtExit,
                                         closed = false,
                                         writer = writer}
   in
      mkOutstream outstream
   end

structure PFS = Posix.FileSys

val stdErr = newOut {appendMode = true, 
                     bufferMode = IO.NO_BUF,
                     closeAtExit = false,
                     fd = PFS.stderr,
                     name = "<stderr>"}

val newOut = fn {appendMode, closeAtExit, fd, name} =>
   newOut {appendMode = appendMode,
           bufferMode = if Posix.ProcEnv.isatty fd
                            then IO.LINE_BUF
                         else IO.BLOCK_BUF,
           closeAtExit = closeAtExit,
           fd = fd,
           name = name}

val stdOut = newOut {appendMode = true,
                     closeAtExit = false,
                     fd = PFS.stdout, 
                     name = "<stdout>"}

val newOut = fn {appendMode, fd, name} =>
   newOut {appendMode = appendMode,
           closeAtExit = true,
           fd = fd,
           name = name}

fun 'a protect' (function: string, name: string, f: unit -> 'a): 'a =
   f () handle e => raise IO.Io {cause = e,
                                 function = function,
                                 name = name}

local
   val readWrite =
      let
         open PFS.S
      in
         flags [irusr, iwusr, irgrp, iwgrp, iroth, iwoth]
      end
in
   fun openOut file =
      protect'
      ("openOut", file, fn () =>
       let
          val fd = PFS.createf (file, Posix.IO.O_WRONLY, 
                                PFS.O.flags (PFS.O.trunc::fileTypeFlags), 
                                readWrite)
       in 
          newOut {fd = fd, 
                  name = file, 
                  appendMode = false}
       end)

   fun openAppend file =
      protect'
      ("openAppend", file, fn () =>
       let
          val fd = PFS.createf (file, Posix.IO.O_WRONLY,
                                PFS.O.flags (PFS.O.append::fileTypeFlags),
                                readWrite)
       in
          newOut {fd = fd, 
                  name = file, 
                  appendMode = true}
       end)
end

val newOut = fn (fd, name) => newOut {fd = fd, 
                                      name = name,
                                      appendMode = false}
val outFd = SIO.outFd o getOutstream

(* ------------------------------------------------- *)
(*                     instream                      *)
(* ------------------------------------------------- *)

datatype state = 
   Closed
 | Open of {eos: bool}
 | Stream of SIO.instream
(* Inv: if !first < !last then !state = Open {eos = false} 
 * if !state = Closed then !first = !last
 * if !state = Open {eos = true} then !first = !last
 *)

datatype instream = In of {augmentedReader: PIO.reader,
                           buf: A.array,
                           first: int ref, (* index of first character *)
                           last: int ref, (* one past the index of the last char *)
                           reader: PIO.reader,
                           state: state ref}

local
   val augmentedReader = PIO.nullRd ()
   val buf = A.arrayUninit 0
   val first = ref 0
   val last = ref 0
   val reader = PIO.nullRd ()
in
   fun mkInstream s = In {augmentedReader = augmentedReader,
                          buf = buf,
                          first = first,
                          last = last,
                          reader = reader,
                          state = ref (Stream s)}
end

fun setInstream (In {first, last, state, ...}, s) =
   (first := 0
    ; last := 0
    ; state := Stream s)

fun equalsIn (In {first = f, ...}, In {first = f', ...}) = f = f'

fun augmentedReaderSel (In {augmentedReader = PIO.RD v, ...}, sel) = sel v

fun readerSel (In {reader = PIO.RD v, ...}, sel) = sel v

fun inbufferName ib = readerSel (ib, #name)

fun inFd ib =
   case readerSel (ib, #ioDesc) of
      NONE => raise IO.Io {cause = Fail "<no ioDesc>",
                           function = "inFd",
                           name = inbufferName ib}
    | SOME ioDesc => valOf (Posix.FileSys.iodToFD ioDesc)

val empty = V.tabulate (0, fn _ => someElem)

local
   fun make (sel, e: exn) ib =
      case augmentedReaderSel (ib, sel) of
         NONE => raise e
       | SOME x => x
in
   val readArr = make (#readArr, IO.BlockingNotSupported)
   val readArrNB = make (#readArrNB, IO.NonblockingNotSupported)
   val readVec = make (#readVec, IO.BlockingNotSupported)
end

fun 'a protect (ib, function: string, f: unit -> 'a): 'a =
   f () handle e => raise IO.Io {cause = e,
                                 function = function,
                                 name = inbufferName ib}

fun update (ib as In {buf, first, last, state, ...}) =
   let
      val i = readArr ib (AS.full buf)
   in
      if i = 0
         then (state := Open {eos = true}
               ; false)
      else (first := 0
            ; last := i
            ; true)
   end

fun input (ib as In {buf, first, last, ...}) =
   let
      val f = !first
      val l = !last
   in
      if f < l
         then (first := l
               ; AS.vector (AS.slice (buf, f, SOME (l - f))))
      else
         let
            val In {state, ...} = ib
         in
            case !state of
               Closed => empty
             | Open {eos} =>
                  if eos
                     then (state := Open {eos = false}
                           ; empty)
                  else protect (ib, "input", fn () =>
                                readVec ib (augmentedReaderSel (ib, #chunkSize)))
             | Stream s =>
                  let
                     val (v, s') = SIO.input s
                     val _ = state := Stream s'
                  in
                     v
                  end
         end
   end

(* input1 will move past a temporary end of stream *)
fun input1 (ib as In {buf, first, last, ...}) =
   let
      val f = !first
   in
      if f < !last
         then (first := f + 1
               ; SOME (A.unsafeSub (buf, f)))
      else
         let
            val In {state, ...} = ib
         in
            case !state of
               Closed => NONE
             | Open {eos} =>
                  if eos
                     then
                        (state := Open {eos = false}
                         ; NONE)
                  else
                     if protect (ib, "input1", fn () => update ib)
                        then
                           (first := 1
                            ; SOME (A.sub (buf, 0)))
                     else NONE
             | Stream s =>
                  let
                     val (c, s') = SIO.input1' s
                     val _ = state := Stream s'
                  in
                     c
                  end
         end
   end

fun inputN (ib as In {buf, first, last, ...}, n) =
   if n < 0 orelse n > V.maxLen
      then raise Size
   else
      let
         val f = !first
         val l = !last
         val size = l - f
      in
         if size >= n
            then (first := f + n
                  ; AS.vector (AS.slice (buf, f, SOME n)))
         else
            let
               val In {state, ...} = ib
            in
               case !state of
                  Closed => empty
                | Open {eos} =>
                     if eos
                        then (state := Open {eos = false}
                              ; empty)
                     else
                        protect
                        (ib, "inputN", fn () =>
                         let
                            val readArr = readArr ib
                            val inp = A.arrayUninit n
                            fun fill k =
                               if k >= size
                                  then ()
                               else (A.update (inp, k, A.sub (buf, f + k))
                                     ; fill (k + 1))
                            val _ = fill 0
                            val _ = first := l
                            fun loop i =
                               if i = n
                                  then i
                               else let
                                       val j = 
                                          readArr
                                          (AS.slice (inp, i, SOME (n - i)))
                                    in
                                       if j = 0
                                          then (state := Open {eos = true}; i)
                                       else loop (i + j)
                                    end
                            val i = loop size
                         in
                            if i = n
                               then V.unsafeFromArray inp
                            else AS.vector (AS.slice (inp, 0, SOME i))
                         end)
                | Stream s =>
                     let
                        val (v, s') = SIO.inputN (s, n)
                        val _ = state := Stream s'
                     in
                        v
                     end
            end
      end

fun inputAll (ib as In {state, ...}) =
   case !state of
      Closed => empty
    | Open {eos} =>
         if eos
            then (state := Open {eos = false}
                  ; empty)
         else
            protect
            (ib, "inputAll", fn () =>
             let
                val In {buf, first, last, ...} = ib
                val readVec = readVec ib
                val f = !first
                val l = !last
                val inp = AS.vector (AS.slice (buf, f, SOME (l - f)))
                val inps = [inp]
                fun loop inps =
                   let
                      val inp =
                         readVec (augmentedReaderSel (ib, #chunkSize))
                   in
                      if V.length inp = 0
                         then V.concat (List.rev inps)
                      else loop (inp :: inps)
                   end
             in
                loop inps
             end)
    | Stream s =>
          let
             val (v, s') = SIO.inputAll s
             val _ = state := Stream s'
          in
             v
          end

val inputLine =
   case line of
      NONE => (fn ib => SOME (input ib))
    | SOME {isLine, lineElem, ...} =>
         let
            val lineVec = V.tabulate (1, fn _ => lineElem)
         in
            fn (ib as In {state, ...}) =>
            case !state of
               Closed => NONE
             | Open {eos} =>
                  if eos
                      then NONE
                  else
                     protect
                     (ib, "inputLine", fn () =>
                      let
                         val In {buf, first, last, ...} = ib
                         fun finish (inps, trail) =
                            let
                               val inps = if trail
                                             then lineVec :: inps
                                          else inps
                               val inp = V.concat (List.rev inps)
                            in
                               SOME inp
                            end
                         fun loop inps =
                            if !first < !last orelse update ib
                               then
                                  let
                                     val f = !first
                                     val l = !last
                                     (* !first < !last *) 
                                     fun loop' i = (* pre: !first <= i <= !last *)
                                        let
                                           fun done j = (* pre: !first < j <= !last *)
                                              let
                                                 val inp = AS.vector (AS.slice (buf, f, SOME (j - f)))
                                              in
                                                 first := j;
                                                 inp::inps
                                              end
                                        in
                                           if i >= l
                                              then loop (done i)
                                           else if isLine (A.sub (buf, i))
                                                   then finish (done (i + 1), false)
                                                else loop' (i + 1)
                                        end
                                  in
                                     loop' f
                                  end
                            else (case inps of
                                     [] => NONE
                                   | _ => finish (inps, true))
                      in
                         loop []
                      end)
             | Stream s =>
                  Option.map
                  (fn (v, s') => (state := Stream s'; v))
                  (SIO.inputLine s)
         end

fun canInput (ib as In {state, ...}, n) =
   if n < 0 orelse n > V.maxLen
      then raise Size
   else
      case !state of
         Closed => SOME 0
       | Open {eos} =>
            if eos
               then SOME 0
            else
               protect
               (ib, "canInput", fn () =>
                let
                   val readArrNB = readArrNB ib
                   val In {buf, first, last, ...} = ib
                   val f = !first
                   val l = !last
                   val read = l - f
                   val _ =
                      if f > 0
                         then
                            (AS.copy {di = 0,
                                      dst = buf,
                                      src = AS.slice (buf, f, SOME read)}
                             ; first := 0)
                      else ()
                   val size = A.length buf
                   (* 0 = !first *)
                   fun loop read =
                      if read = size
                         then read
                      else
                         let
                            val slice = AS.slice (buf, read, NONE)
                            val i = readArrNB slice
                         in
                            case i of
                               NONE => read
                             | SOME i =>
                                  if 0 = i then read else loop (read + i)
                         end
                   val read = loop read
                   val _ = last := read
                in
                   SOME (if read > 0
                            then Int.min (n, read)
                         else (state := Open {eos = true}; 0))
                end)
       | Stream s => SIO.canInput (s, n)

fun lookahead (ib as In {buf, first, last, ...}) =
   let
      val f = !first
      val l = !last
   in
      if f < l
         then SOME (A.unsafeSub (buf, f))
      else
         let
            val In {state, ...} = ib
         in
            case !state of
               Closed => NONE
             | Open {eos, ...} =>
                  if eos
                     then NONE
                  else if protect (ib, "lookahead", fn () => update ib)
                          then SOME (A.sub (buf, 0))
                       else NONE
             | Stream s => Option.map #1 (SIO.input1 s)
         end
   end

fun closeIn (ib as In {first, last, state, ...}) =
   case !state of
      Closed => ()
    | Open _ =>
         (first := !last
          ; state := Closed
          ; protect (ib, "closeIn", fn () => readerSel (ib, #close) ()))
    | Stream s => SIO.closeIn s

fun endOfStream (ib as In {first, last, state, ...}) =
   !first = !last
   andalso
   (case !state of
       Closed => true
     | Open {eos, ...} =>
          eos orelse not (protect (ib, "endOfStream", fn () => update ib))
     | Stream s => SIO.endOfStream s)

fun mkInbuffer' {reader, closed, bufferContents} =
   let
      val (state, first, last, buf) =
         if closed
            then (ref Closed, ref 0, ref 0, Array.array (0, someElem))
         else let
                 val PIO.RD {chunkSize, ...} = reader
                 val buf = Array.array (chunkSize, someElem)
                 val first = ref 0
                 val (state, last) =
                    case bufferContents of
                       NONE => (ref (Open {eos = false}), ref 0)
                     | SOME v =>
                          if V.length v = 0
                             then (ref (Open {eos = true}), ref 0)
                          else (V.appi (fn (i, c) => A.update (buf, i, c)) v
                                ; (ref (Open {eos = false}), ref (V.length v)))
              in
                 (state, first, last, buf)
              end
   in
      In {augmentedReader = PIO.augmentReader reader,
          buf = buf,
          first = first,
          last = last,
          reader = reader,
          state = state}
   end

fun openVector v = 
   mkInbuffer' {bufferContents = NONE,
                closed = false,
                reader = PIO.openVector v}

val openInbuffers : (instream * {close: bool}) list ref = ref []

fun getInstream (ib as In {state, ...}) =
   let
      fun doit (closed: bool, bufferContents) =
         let
            val In {reader, ...} = ib
            val (ibs, openInbuffers') =
               List.partition (fn (ib', _) => equalsIn (ib, ib'))
               (!openInbuffers)
            val _ = openInbuffers := openInbuffers'
            val closeAtExit = 
               List.foldr (fn ((_, {close = close'}), close) =>
                           close orelse close')
               false ibs
         in
            SIO.mkInstream'' {bufferContents = bufferContents,
                              closeAtExit = closeAtExit,
                              closed = closed,
                              reader = reader}
         end
   in
      case !state of
         Closed => doit (true, NONE)
       | Open {eos} =>
            if eos
               then doit (false, SOME (true, empty))
            else
               let
                  val In {buf, first, last, ...} = ib
                  val f = !first
                  val l = !last
                  val s =
                     if f < l
                        then 
                           doit (false,
                                 SOME (true, 
                                       AS.vector (AS.slice (buf, f, 
                                                            SOME (l - f)))))
                        else doit (false, NONE)
                  val () = state := Stream s
               in
                  s
               end
       | Stream s => s
   end

val mkInbuffer'' =
   let
      val _ =
         Cleaner.addNew
         (Cleaner.atExit, fn () =>
          List.app (fn (ib, {close}) => if close then closeIn ib else ())
          (!openInbuffers))
   in
      fn {bufferContents, closeAtExit, closed, reader} =>
      let
         val ib = mkInbuffer' {bufferContents = bufferContents,
                               closed = closed,
                               reader = reader}
         val _ = if closed
                    then ()
                 else openInbuffers := ((ib, {close = closeAtExit})
                                        :: (!openInbuffers))
      in
         ib
      end
   end

fun scanStream f is =
   case f SIO.input1 (getInstream is) of
      NONE => NONE
    | SOME (v, s') => (setInstream (is, s'); SOME v)

val closeIn = fn ib =>
   let
      val _ = openInbuffers := List.filter (fn (ib',_) => 
                                            not (equalsIn (ib, ib'))) 
         (!openInbuffers)
   in
      closeIn ib
   end

fun newIn {bufferContents, closeAtExit, fd, name} =
   let 
      val reader = mkReader {fd = fd, initBlkMode = true, name = name}
   in
      mkInbuffer'' {bufferContents = bufferContents,
                    closeAtExit = closeAtExit,
                    closed = false,
                    reader = reader}
   end

val newIn = fn (fd, name) =>
   newIn {bufferContents = NONE,
          closeAtExit = true,
          fd = fd,
          name = name}

val stdIn = newIn (PFS.stdin, "<stdin>")

fun openIn file =
   protect'
   ("openIn", file, fn () =>
    let 
       val fd = PFS.openf (file, Posix.IO.O_RDONLY, PFS.O.flags fileTypeFlags)
    in 
       newIn (fd, file)
    end)

end

signature IMPERATIVE_IO_ARG =
   sig
      structure Array: MONO_ARRAY
(*      structure ArraySlice: MONO_ARRAY_SLICE *)
      structure StreamIO: STREAM_IO
      structure Vector: MONO_VECTOR
(*      structure VectorSlice: MONO_VECTOR_SLICE *)
(*      sharing type Array.array = ArraySlice.array *)
      sharing type
         Array.elem
(*       = ArraySlice.elem *)
         = StreamIO.elem
         = Vector.elem
(*       = VectorSlice.elem *)
      sharing type
         Array.vector
(*       = ArraySlice.vector *)
         = Vector.vector
(*       = VectorSlice.vector *)
(*      sharing type ArraySlice.vector_slice = VectorSlice.slice *)
   end

functor ImperativeIO (S: IMPERATIVE_IO_ARG): IMPERATIVE_IO =
   struct
      open S

      structure SIO = StreamIO

      type elem = SIO.elem
      type vector = SIO.vector

      datatype outstream = Out of SIO.outstream ref

      fun output (Out os, v) = SIO.output (!os, v)
      fun output1 (Out os, v) = SIO.output1 (!os, v)
      fun flushOut (Out os) = SIO.flushOut (!os)
      fun closeOut (Out os) = SIO.closeOut (!os)
      fun mkOutstream os = Out (ref os)
      fun getOutstream (Out os) = !os
      fun setOutstream (Out os, os') = os := os'
      fun getPosOut (Out os) = SIO.getPosOut (!os)
      fun setPosOut (Out os, out_pos) = os := SIO.setPosOut out_pos

      datatype instream = In of SIO.instream ref

      fun canInput (In is, n) = SIO.canInput (!is, n)
      fun closeIn (In is) = SIO.closeIn (!is)
      fun endOfStream (In is) = SIO.endOfStream (!is)
      fun getInstream (In is) = !is
      fun input (In is) = let val (v, is') = SIO.input (!is)
                          in is := is'; v
                          end 
      (* input1 will never move past a temporary end of stream *)
      fun input1 (In is) = 
        case SIO.input1 (!is) of
          SOME (c,is') => (is := is'; SOME c)
        | NONE => NONE
      fun inputAll (In is) = let val (v, is') = SIO.inputAll (!is)
                             in is := is'; v
                             end
      fun inputN (In is, n) = let val (v, is') = SIO.inputN (!is, n)
                              in is := is'; v
                              end
      fun lookahead (In is) =
         Option.map #1 (SIO.input1 (!is))
      fun mkInstream is = In (ref is)
      fun setInstream (In is, is') = is := is'
   end
