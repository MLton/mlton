(* Copyright (C) 2002-2003 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

structure BinPrimIO : PRIM_IO
                      where type array = Word8Array.array
		      where type vector = Word8Vector.vector
		      where type elem = Word8.word
		      where type pos = Position.int =
  PrimIO (structure Vector = Word8Vector
	  structure VectorSlice = Word8VectorSlice
	  structure Array = Word8Array
	  structure ArraySlice = Word8ArraySlice
	  type pos = Position.int
	  val compare = Position.compare
	  val someElem = 0wx0: Word8.word)
