(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh Jagannathan, and
 *    Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
local
   type int = Int.t
   type word = Word.t
   structure All:>
      sig
	 type bytes
	 type words
	    
	 structure Bits:
	    sig
	       eqtype t

	       val + : t * t -> t
	       val - : t * t -> t
	       val < : t * t -> bool
	       val <= : t * t -> bool
	       val > : t * t -> bool
	       val >= : t * t -> bool
	       val compare: t * t -> Relation.t
	       val equals: t * t -> bool
	       val fromInt: int -> t
	       val fromIntInf: IntInf.t -> t
	       val fromWord: word -> t
	       val inByte: t
	       val inPointer: t
	       val inWord: t
	       val isByteAligned: t -> bool
	       val isWordAligned: t -> bool
	       val isZero: t -> bool
	       val layout: t -> Layout.t
	       val toBytes: t -> bytes
	       val toInt: t -> int
	       val toIntInf: t -> IntInf.t
	       val toString: t -> string
	       val toWord: t -> word
	       val zero: t
	    end
	 
	 structure Bytes:
	    sig
	       type t

	       val + : t * t -> t
	       val - : t * t -> t
	       val ~ : t -> t
	       val < : t * t -> bool
	       val <= : t * t -> bool
	       val > : t * t -> bool
	       val >= : t * t -> bool
	       val align: t * {alignment: t} -> t
	       val equals: t * t -> bool
	       val fromInt: int -> t
	       val fromIntInf: IntInf.t -> t
	       val fromWord: word -> t
	       val inPointer: t
	       val inWord: t
	       val isWordAligned: t -> bool
	       val isZero: t -> bool
	       val layout: t -> Layout.t
	       val max: t * t -> t
	       val scale: t * int -> t
	       val toBits: t -> Bits.t
	       val toInt: t -> int
	       val toIntInf: t -> IntInf.t
	       val toString: t -> string
	       val toWord: t -> word
	       val toWords: t -> words
	       val wordAlign: t -> t
	       val zero: t
	    end
	 
	 structure Words:
	    sig
	       type t

	       val layout: t -> Layout.t
	       val toInt: t -> int
	       val toBytes: t -> Bytes.t
	    end
	 
	 sharing type bytes = Bytes.t
         sharing type words = Words.t
      end =
      struct
	 val rem = IntInf.rem
	    
	 fun align (b, {alignment = a}) =
	    let
	       val b = b + (a - 1)
	    in
	       b - rem (b, a)
	    end

	 structure Bits =
	    struct
	       open IntInf

	       val fromWord = Word.toIntInf
		  
	       val inByte: t = 8
		  
	       val inWord: t = 32

	       val inPointer = inWord

	       fun isByteAligned b = 0 = rem (b, inByte)
		  
	       fun isWordAligned b = 0 = rem (b, inWord)
		  
	       fun toBytes b =
		  if isByteAligned b
		     then quot (b, inByte)
		  else Error.bug "Bits.toBytes"

	       val toWord = Word.fromIntInf
	    end

	 structure Bytes =
	    struct
	       open IntInf

	       val fromWord = Word.toIntInf

	       val inWord: t = 4

	       val inPointer = inWord

	       fun isWordAligned b = 0 = rem (b, inWord)

	       fun scale (b, i) = b * Int.toIntInf i
		  
	       fun toBits b = b * Bits.inByte

	       val toWord = Word.fromIntInf

	       fun toWords b =
		  if isWordAligned b
		     then quot (b, inWord)
		  else Error.bug "Bytes.toWords"

	       val align = align

	       fun wordAlign b = align (b, {alignment = inWord})
	    end

	 type bytes = Bytes.t

	 structure Words =
	    struct
	       open IntInf

	       fun toBytes w = w * Bytes.inWord
	    end

	 type words = Words.t
      end
   open All
in
   structure Bits = Bits
   structure Bytes = Bytes
   structure Words = Words
end
