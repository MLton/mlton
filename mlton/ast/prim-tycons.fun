(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
functor PrimTycons (S: PRIM_TYCONS_STRUCTS): PRIM_TYCONS =
struct

open S

datatype z = datatype IntSize.t
datatype z = datatype RealSize.t
datatype z = datatype WordSize.t

type tycon = t

val array = fromString "array"
val arrow = fromString "->"
val bool = fromString "bool"
val char = fromString "char"
val exn = fromString "exn"
val int8 = fromString "int8"
val int16 = fromString "int16"
val int32 = fromString "int32"
val int64 = fromString "int64"
val intInf = fromString "intInf"
val list = fromString "list"
val pointer = fromString "pointer"
val preThread = fromString "preThread"
val real32 = fromString "real32"
val real64 = fromString "real64"
val reff = fromString "ref"
val thread = fromString "thread"
val tuple = fromString "*"
val vector = fromString "vector"
val weak = fromString "weak"
val word8 = fromString "word8"
val word16 = fromString "word16"
val word32 = fromString "word32"
val word64 = fromString "word64"

val ints =
   [(int8, I8),
    (int16, I16),
    (int32, I32),
    (int64, I64)]

val reals =
   [(real32, R32),
    (real64, R64)]

val words =
   [(word8, W8),
    (word16, W16),
    (word32, W32),
    (word64, W64)]

datatype z = datatype Kind.t
datatype z = datatype AdmitsEquality.t
   
val prims =
   [(array, Arity 1, Always),
    (arrow, Arity 2, Never),
    (bool, Arity 0, Always),
    (char, Arity 0, Always),
    (exn, Arity 0, Never),
    (int8, Arity 0, Always),
    (int16, Arity 0, Always),
    (int32, Arity 0, Always),
    (int64, Arity 0, Always),
    (intInf, Arity 0, Always),
    (list, Arity 1, Sometimes),
    (pointer, Arity 0, Always),
    (preThread, Arity 0, Never),
    (real32, Arity 0, Never),
    (real64, Arity 0, Never),
    (reff, Arity 1, Always),
    (thread, Arity 0, Never),
    (tuple, Nary, Sometimes),
    (vector, Arity 1, Sometimes),
    (weak, Arity 1, Never),
    (word8, Arity 0, Always),
    (word16, Arity 0, Always),
    (word32, Arity 0, Always),
    (word64, Arity 0, Always)]
   
val int =
   fn I8 => int8
    | I16 => int16
    | I32 => int32
    | I64 => int64

val real =
   fn R32 => real32
    | R64 => real64
	
val word =
   fn W8 => word8
    | W16 => word16
    | W32 => word32
    | W64 => word64
	 
val defaultInt = int IntSize.default
val defaultReal = real RealSize.default
val defaultWord = word WordSize.default
   
local
   fun is l t = List.exists (l, fn t' => equals (t, t'))
in
   val isIntX = is [int8, int16, int32, int64, intInf]
   val isRealX = is [real32, real64]
   val isWordX = is [word8, word16, word32, word64]
end

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
		  0 => ({isChar = equals (c, char)}, layout c)
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
	  
