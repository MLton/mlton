(*
structure TextIO: TEXT_IO_EXTRA =
   struct
      structure S = struct
		      structure PrimIO = TextPrimIO
		      structure Array = CharArray
		      structure Vector = CharVector
		      val someElem = (#"\000": Char.char)
		      val lineElem = (#"\n": Char.char)
		      fun isLine c = c = lineElem
		      val hasLine = CharVector.exists isLine
		      structure Cleaner = Cleaner
		    end
      structure StreamIO = StreamIOExtraFile(open S)
      structure SIO = StreamIO
      structure S = struct 
		      open S 
		      structure StreamIO = StreamIO
		    end
      structure BufferI = BufferIExtraFile(open S)
      structure BI = BufferI
      structure S = struct
		      open S
		      structure BufferI = BufferI
		      val chunkSize = Primitive.TextIO.bufSize
		      val fileTypeFlags = [PosixPrimitive.FileSys.O.text]
		      val mkReader = Posix.IO.mkTextReader
		      val mkWriter = Posix.IO.mkTextWriter
		    end
      structure ImperativeIO = ImperativeIOExtraFile(open S)
      structure FastImperativeIO = FastImperativeIOExtraFile(open S)
      open FastImperativeIO

      structure StreamIO =
	 struct
	    open SIO
	    val outputSubstr = fn (os, ss) => 
	      let
		val (s, i, sz) = Substring.base ss
	      in
		outputSlice (os, (s, i, SOME sz))
	      end
	 end

      val outputSubstr = fn (os, ss) => 
	let
	  val (s, i, sz) = Substring.base ss
	in
	  outputSlice (os, (s, i, SOME sz))
	end
      val openString = openVector
      fun print (s: string) = (output (stdOut, s); flushOut stdOut)
   end
*)

structure TextIO : TEXT_IO_EXTRA =
   struct
      structure NativeVector =
	 struct
	    type vector = string
	    type elem = char

	    val toByte = Byte.charToByte
	    val fromByte = Byte.byteToChar
	    val fromWord8Vector = Primitive.String.fromWord8Vector
	    val toWord8Vector = Primitive.String.toWord8Vector
	    val concat = String.concat
	    val empty = ""
	    fun isEmpty s = s = ""
	    fun hasLine s = Char.contains s #"\n"
	    fun isLine c = c = #"\n"
	 end

      local
	 structure PreTextIO =
	    BinOrTextIO (val fileTypeFlags = [PosixPrimitive.FileSys.O.text]
			 structure Cleaner = Cleaner
			 structure Int = Int
			 structure NativeVector = NativeVector
			 structure Primitive = Primitive
			 structure String = String)
      in
	 open PreTextIO
      end

      structure String =
	 struct
	    open Primitive.String
	    open String
	 end

      fun outputSubstr (out, ss): unit = output (out, Substring.string ss)
	 
      fun print (s: string) = (output (stdOut, s); flushOut stdOut)

      structure Buf =
	 struct
	    open Buf
	       
	    fun lastChar s = String.sub (s, String.size s - 1)

	    fun inputLine (b as T {eof, buf, first, last, ...}) =
	       let
		  fun loop (ac: string list) =
		     if update (b, "inputLine")
			then
			   let
			      (* !first < !last *)
			      fun loop' i = (* pre: !first <= i <= !last *)
				 let
				    val f = !first
				    fun done j = (* pre: !first < j <= !last *)
				       (first := j
					; (String.fromWord8Vector
					   (Array.extract (buf, f, SOME (j -? f)))
					   :: ac))
				 in if i >= !last
				       then loop (done i)
				    else (case Byte.byteToChar (Array.sub (buf, i)) of
					     #"\n" => done (i + 1)
					   | _ => loop' (i + 1))
				 end
			   in loop' (! first)
			   end
		     else (eof := false; ac)
		  val ac = loop []
	       in
		  case ac of
		     [] => ""
		   | s :: _ => concat (rev (case lastChar s of
					       #"\n" => ac
					     | _ => "\n" :: ac))
	       end
	 end

      structure StreamIO =
	 struct
	    open StreamIO

	    fun inputLine (s: t) =
	       let
		  fun loop (s: t, ac: char list): string * t =
		     case input1 s of
			NONE =>
			   let fun done ac = (implode (rev ac), s)
			   in case ac of
			      [] => ("", s)
			    | #"\n" :: _ => done ac
			    | _ => done (#"\n" :: ac)
			   end
		      | SOME (c, s) =>
			   if c = #"\n"
			      then (implode (rev (#"\n" :: ac)), s)
			   else loop (s, c :: ac)
	       in loop (s, [])
	       end

	 end

      fun inputLine (T r): string =
	 case !r of
	    Buf b => Buf.inputLine b
	  | Stream s => let val (res, s) = StreamIO.inputLine s
			in r := Stream s; res
			end
   end

structure TextIOGlobal: TEXT_IO_GLOBAL = TextIO
open TextIOGlobal
