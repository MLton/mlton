signature PRIM_IO_ARG =
   sig
      structure Array: MONO_ARRAY
      structure ArraySlice: MONO_ARRAY_SLICE
      structure Vector: MONO_VECTOR
      structure VectorSlice: MONO_VECTOR_SLICE
      sharing type Array.array = ArraySlice.array
      sharing type Array.elem = ArraySlice.elem = Vector.elem = VectorSlice.elem
      sharing type Array.vector = ArraySlice.vector = Vector.vector
	 = VectorSlice.vector
      sharing type ArraySlice.vector_slice = VectorSlice.slice
	
      eqtype pos

      val compare: (pos * pos) -> order
      val someElem: Vector.elem
   end

functor PrimIO (S: PRIM_IO_ARG): PRIM_IO = 
struct

open S

structure A = Array
structure AS = ArraySlice
structure V = Vector
structure VS = VectorSlice

type array = A.array
type array_slice = AS.slice
type vector = V.vector
type vector_slice = VS.slice
type elem = A.elem
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
      val empty = V.tabulate (0, fn _ => someElem)
      val pos = ref 0
      val eofPos = V.length v
      fun check f = if !closed
		       then liftExn name f IO.ClosedStream
		    else ()
      fun const f c = fn _ => (check f; c)
      fun function f g = fn x => (check f; g x)
      fun readVec f i =
	 let
	    val _ = check f
	    val n = Int.min (i, eofPos - !pos)
	 in
	    V.tabulate (n, fn i => (V.sub (v, !pos)) before (pos := !pos + 1))
	 end
      fun readArr f sl =
	 let
	    val _ = check f
	    val (buf, i, sz) = ArraySlice.base sl
	    val n = Int.min (sz, eofPos - !pos)
	    fun loop j = if j >= n
			    then ()
			 else (A.update (buf, i + j, V.sub (v, !pos));
			       pos := !pos + 1;
			       loop (j + 1))
	 in
	    loop 0;
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
      fun function f g = fn x => (check f; g x)
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
      fun sizeS (base, length) sl =
	 let
	    val (buf, i, sz) = base sl
	 in
	    Int.min (length buf - i, sz)
	 end
      val sizeA = sizeS (AS.base, A.length)
      val sizeV = sizeS (VS.base, V.length)
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
	  writeArr = SOME (function "writeArr" sizeA),
	  writeArrNB = SOME (function "writeArrNB" (SOME o sizeA)),
	  writeVec = SOME (function "writeVec" sizeV),
	  writeVecNB = SOME (function "writeVecNB" (SOME o sizeV))}
   end

fun doBlock (f, block) x = (block (); valOf (f x))
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
			 val k = readArr (AS.slice (buf, 0, SOME i))
		      in
			 V.tabulate (k, fn i => A.sub (buf, i))
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
      val ((writeVec,writeVecNB),(writeArr,wriveArrNB)) =
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
