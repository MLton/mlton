structure TextIO: TEXT_IO_EXTRA =
   struct
      structure StreamIO = StreamIOExtra(structure PrimIO = TextPrimIO
					 structure Array = CharArray
					 structure Vector = CharVector
					 val someElem = (#"\000": Char.char))
      structure ImperativeIO = ImperativeIOExtra(structure StreamIO = StreamIO
						 structure Vector = CharVector)
      open ImperativeIO
      structure StreamIO =
	 struct
	    open StreamIO
	    val inputLine = fn _ => raise (Fail "<unimplemented>")
	    val outputSubstr = fn _ => raise (Fail "<unimplemented>")
	 end

      val inputLine = fn _ => raise (Fail "<unimplemented>")
      val outputSubstr = fn _ => raise (Fail "<unimplemented>")
      val openIn = fn _ => raise (Fail "<unimplemented>")
      val openOut = fn _ => raise (Fail "<unimplemented>")
      val openAppend = fn _ => raise (Fail "<unimplemented>")
      val openString = fn _ => raise (Fail "<unimplemented>")
      val stdIn = (mkInstream o StreamIO.mkInstream) 
	          (TextPrimIO.nullRd (), CharVector.tabulate (0, fn _ => #"\000"))
      val stdOut = (mkOutstream o StreamIO.mkOutstream) 
	           (TextPrimIO.nullWr (), IO.NO_BUF)
      val stdErr = (mkOutstream o StreamIO.mkOutstream) 
	           (TextPrimIO.nullWr (), IO.NO_BUF)
      val print = fn _ => raise (Fail "<unimplemented>")
      val scanStream = fn _ => raise (Fail "<unimplemented>")
   end

structure TextIOGlobal: TEXT_IO_GLOBAL = TextIO
open TextIOGlobal

(*
(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
*)