signature PRIM_IO = 
   sig
      type array
      type vector
      type elem
      eqtype pos
      val compare: pos * pos -> order
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
      val openVector: vector -> reader
      val nullRd: unit -> reader
      val nullWr: unit -> writer
      val augmentReader: reader -> reader
      val augmentWriter: writer -> writer
   end

signature PRIM_IO_EXTRA = 
   sig
      include PRIM_IO
   end