signature PRIM_IO_ARG =
   sig
     structure A: MONO_ARRAY
     structure V: MONO_VECTOR
     sharing type A.vector = V.vector
     sharing type A.elem = V.elem
     val someElem : A.elem
     eqtype pos
     val compare : (pos * pos) -> order
   end

functor PrimIO (S : PRIM_IO_ARG): PRIM_IO = 
   struct
      open S

      type array = A.array
      type vector = V.vector
      type elem = A.elem
      type pos = pos
      val compare = compare

      datatype reader = RD of {name: string,
			       chunkSize: int,
			       readVec: (int -> vector) option,
			       readArr: ({buf: array, 
					  i: int,
					  sz: int option} -> int) option,
			       readVecNB: (int -> vector option) option,
			       readArrNB: ({buf: array, 
					    i: int, 
					    sz: int option} -> int option) option,
			       block: (unit -> unit) option,
			       canInput: (unit -> bool) option,
			       avail: unit -> int option,
			       getPos: (unit -> pos) option,
			       setPos: (pos -> unit) option,
			       endPos: (unit -> pos) option,
			       verifyPos: (unit -> pos) option,
			       close: unit -> unit,
			       ioDesc: OS.IO.iodesc option}
      datatype writer = WR of {name: string,
			       chunkSize: int,
			       writeVec: ({buf: vector, 
					   i: int, 
					   sz: int option} -> int) option,
			       writeArr: ({buf: array, 
					   i: int, 
					   sz: int option} -> int) option,
			       writeVecNB: ({buf: vector, 
					     i: int, 
					     sz: int option} -> int option) option,
			       writeArrNB: ({buf: array, 
					     i: int, 
					     sz: int option} -> int option) option,
			       block: (unit -> unit) option,
			       canOutput: (unit -> bool) option,
			       getPos: (unit -> pos) option,
			       setPos: (pos -> unit) option,
			       endPos: (unit -> pos) option,
			       verifyPos: (unit -> pos) option,
			       close: unit -> unit,
			       ioDesc: OS.IO.iodesc option}

      fun liftExn name function cause = raise IO.Io {name = name,
						     function = function,
						     cause = cause}
      fun openVector v =
	let
	  val name = "openVector"
	  val closed = ref false
	  val empty = V.fromList []
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
	  fun readArr f {buf, i, sz} =
	    let
	      val _ = check f
	      val n = Int.min (case sz of
				 SOME sz => sz
			       | NONE => A.length buf - i, eofPos - !pos)
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
	  RD {name = name,
	      chunkSize = 32,
	      readVec = SOME (readVec "readVec"),
	      readArr = SOME (readArr "readArr"),
	      readVecNB = SOME (SOME o (readVec "readVecNB")),
	      readArrNB = SOME (SOME o (readArr "readVecNB")),
	      block = SOME (const "block" ()),
	      canInput = SOME (const "canInput" true),
	      avail = const "avail" NONE,
	      getPos = NONE,
	      setPos = NONE,
	      endPos = NONE,
	      verifyPos = NONE,
	      close = fn () => (closed := true),
	      ioDesc = NONE}
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
	  RD {name = name,
	      chunkSize = 1,
	      readVec = SOME (const "readVec" empty),
	      readArr = SOME (const "readArr" 0),
	      readVecNB = SOME (const "readVecNB" (SOME empty)),
	      readArrNB = SOME (const "readArrNB" (SOME 0)),
	      block = SOME (const "block" ()),
	      canInput = SOME (const "canInput" true),
	      avail = const "avail" NONE,
	      getPos = NONE,
	      setPos = NONE,
	      endPos = NONE,
	      verifyPos = NONE,
	      close = fn () => (closed := true),
	      ioDesc = NONE}
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
	  fun sizeS length {buf, i, sz} =
	    case (length buf, sz) of
	      (len, NONE) => len - i
	    | (len, SOME sz) => Int.min (len - i, sz)
	  val sizeA = sizeS A.length
	  val sizeV = sizeS V.length
	in
	  WR {name = name,
	      chunkSize = 1,
	      writeVec = SOME (function "writeVec" sizeV),
	      writeArr = SOME (function "writeArr" sizeA),
	      writeVecNB = SOME (function "writeVecNB" (SOME o sizeV)),
	      writeArrNB = SOME (function "writeArrNB" (SOME o sizeA)),
	      block = SOME (const "block" ()),
	      canOutput = SOME (const "canOutput" true),
	      getPos = NONE,
	      setPos = NONE,
	      endPos = NONE,
	      verifyPos = NONE,
	      close = fn () => (closed := true),
	      ioDesc = NONE}
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
			 val k = readArr {buf = buf, i = 0, sz = SOME i}
		       in
			 V.tabulate (k, fn i => A.sub (buf, i))
		       end),
		 SOME readArr)
	    | (SOME readVec, NONE) =>
		(SOME readVec,
		 SOME (fn {buf, i, sz} =>
		       let
			 val v = readVec (case sz of 
					    SOME sz => sz 
					  | NONE => A.length buf - i)
			 val k = V.length v
			 val _ = A.copyVec {src = v, dst = buf, di = i}
		       in
			 k
		       end))
	    | (NONE, NONE) => (NONE, NONE)
	  fun augmentReadNB (readVecNB, readArrNB) =
	    case (readVecNB, readArrNB) of
	      (SOME readVecNB, SOME readArrNB) => (SOME readVecNB, SOME readArrNB)
	    | (NONE, SOME readArrNB) => 
		(SOME (fn i =>
		       let
			 val buf = A.array (i, someElem)
		       in
			 case readArrNB {buf = buf, i = 0, sz = SOME i} of
			   NONE => NONE
			 | SOME k => SOME (V.tabulate (k, fn i => A.sub (buf, i)))
		       end),
		 SOME readArrNB)
	    | (SOME readVecNB, NONE) =>
		(SOME readVecNB,
		 SOME (fn {buf, i, sz} =>
		       case readVecNB (case sz of
					 SOME sz => sz
				       | NONE => A.length buf - i) of
			 NONE => NONE
		       | SOME v => let
				     val k = V.length v
				     val _ = A.copyVec {src = v, dst = buf, di = i}
				   in
				     SOME k
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
		(SOME (fn {buf, i, sz} =>
		       writeArr {buf = A.tabulate (V.length buf, 
						   fn i => V.sub (buf, i)),
				 i = i, sz = sz}),
		 SOME writeArr)
	    | (SOME writeVec, NONE) =>
		(SOME writeVec,
		 SOME (fn {buf, i, sz} =>
		       writeVec {buf = A.vector buf, i = i, sz = sz}))
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
