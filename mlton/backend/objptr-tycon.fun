(* Copyright (C) 2009,2019-2020 Matthew Fluet.
 * Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ObjptrTycon (S: OBJPTR_TYCON_STRUCTS): OBJPTR_TYCON =
struct

open S

datatype t = T of {index: int ref}

local
   fun make f (T r) = f r
in
   val index = ! o (make #index)
end

local
   val c = Counter.generator 0
in
   fun new () = T {index = ref (c ())}
end

fun setIndex (T {index = r}, i) = r := i

fun fromIndex i = T {index = ref i}

fun compare (opt, opt') = Int.compare (index opt, index opt')

fun equals (opt, opt') = index opt = index opt'

val op <= = fn (opt, opt') => index opt <= index opt'

fun toString (opt: t): string =
   concat ["opt_", Int.toString (index opt)]

val layout = Layout.str o toString

fun toHeader (opt: t): WordX.t =
   WordX.fromWord (Runtime.typeIndexToHeader (index opt), WordSize.objptrHeader ())

val stack = new ()
val thread = new ()
val weakGone = new ()

local
   val real32Vector = new ()
   val real64Vector = new ()
in
   fun realVector (rs: RealSize.t): t =
      case rs of
         RealSize.R32 => real32Vector
       | RealSize.R64 => real64Vector
end

local
   val word8Vector = new ()
   val word16Vector = new ()
   val word32Vector = new ()
   val word64Vector = new ()
in
   fun wordVector (ws: WordSize.t): t =
      case WordSize.primOpt ws of
         SOME WordSize.W8 => word8Vector
       | SOME WordSize.W16 => word16Vector
       | SOME WordSize.W32 => word32Vector
       | SOME WordSize.W64 => word64Vector
       | _ => Error.bug "ObjptrTycon.wordVector"
end

fun hash (T {index}) = (Hash.permute o Word.fromInt o !) index

end
