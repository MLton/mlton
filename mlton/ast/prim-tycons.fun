(* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor PrimTycons (S: PRIM_TYCONS_STRUCTS): PRIM_TYCONS =
struct

open S

datatype z = datatype RealSize.t

type tycon = t

val array = fromString "array"
val arrow = fromString "->"
val bool = fromString "bool"
val exn = fromString "exn"
val intInf = fromString "intInf"
val list = fromString "list"
val pointer = fromString "pointer"
val preThread = fromString "preThread"
val reff = fromString "ref"
val thread = fromString "thread"
val tuple = fromString "*"
val vector = fromString "vector"
val weak = fromString "weak"

datatype z = datatype Kind.t
datatype z = datatype AdmitsEquality.t

local
   fun 'a make (prefix: string,
		all: 'a list,
		bits: 'a -> Bits.t,
		default: 'a,
		equalsA: 'a * 'a -> bool,
		memo: ('a -> t) -> ('a -> t),
		admitsEquality: AdmitsEquality.t) =
      let
	 val all =
	    Vector.fromListMap
	    (all, fn s =>
	     (fromString (concat [prefix, Bits.toString (bits s)]), s))
	 val fromSize =
	    memo
	    (fn s =>
	     case Vector.peek (all, fn (_, s') => equalsA (s, s')) of
		NONE => Error.bug "missing size"
	      | SOME (tycon, _) => tycon)
	 fun is t = Vector.exists (all, fn (t', _) => equals (t, t'))
	 val prims =
	    Vector.toListMap (all, fn (tycon, _) =>
			      (tycon, Arity 0, admitsEquality))
      in
	 (fromSize default, fromSize, all, is, prims)
      end
in
   val (defaultChar, char, chars, isCharX, primChars) =
      let
	 open CharSize
      in
	 make ("char", all, bits, default, equals, memoize, Sometimes)
      end
   val (defaultInt, int, ints, isIntX, primInts) =
      let
	 open IntSize
      in
	 make ("int", all, bits, default, equals, memoize, Sometimes)
      end
   val (defaultReal, real, reals, isRealX, primReals) =
      let
	 open RealSize
      in
	 make ("real", all, bits, default, equals, memoize, Never)
      end
   val (defaultWord, word, words, isWordX, primWords) =
      let
	 open WordSize
      in
	 make ("word", all, bits, default, equals, memoize, Sometimes)
      end
end

val isIntX = fn c => equals (c, intInf) orelse isIntX c

val prims =
   [(array, Arity 1, Always),
    (arrow, Arity 2, Never),
    (bool, Arity 0, Sometimes),
    (exn, Arity 0, Never),
    (intInf, Arity 0, Sometimes),
    (list, Arity 1, Sometimes),
    (preThread, Arity 0, Never),
    (reff, Arity 1, Always),
    (thread, Arity 0, Never),
    (tuple, Nary, Sometimes),
    (vector, Arity 1, Sometimes),
    (weak, Arity 1, Never)]
   @ primChars @ primInts @ primReals @ primWords

fun layoutApp (c: t,
	       args: (Layout.t * {isChar: bool, needsParen: bool}) vector) =
   let
      local
	 open Layout
      in
	 val mayAlign = mayAlign
	 val seq = seq
	 val str = str
      end
      fun maybe (l, {isChar = _, needsParen}) =
	 if needsParen
	    then Layout.paren l
	 else l
      fun normal () =
	 let
	    val ({isChar}, lay) =
	       case Vector.length args of
		  0 => ({isChar = equals (c, defaultChar)}, layout c)
		| 1 => ({isChar = false},
			seq [maybe (Vector.sub (args, 0)), str " ", layout c])
		| _ => ({isChar = false},
			seq [Layout.tuple (Vector.toListMap (args, maybe)),
			     str " ", layout c])
	 in
	    (lay, {isChar = isChar, needsParen = false})
	 end
   in
      if equals (c, arrow)
	 then (mayAlign [maybe (Vector.sub (args, 0)),
			 seq [str "-> ", maybe (Vector.sub (args, 1))]],
	       {isChar = false, needsParen = true})
      else if equals (c, tuple)
         then if 0 = Vector.length args
		 then (str "unit", {isChar = false, needsParen = false})
	      else (mayAlign (Layout.separateLeft
			      (Vector.toListMap (args, maybe), "* ")),
		    {isChar = false, needsParen = true})
      else if equals (c, vector)
         then if #isChar (#2 (Vector.sub (args, 0)))
		 then (str "string", {isChar = false, needsParen = false})
	      else normal ()
      else normal ()
   end

end
	  
