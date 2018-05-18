(* Copyright (C) 2009 Matthew Fluet.
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
   val c = Counter.new 0
in
   fun new () = T {index = ref (Counter.next c)}
end

fun setIndex (T {index = r}, i) = r := i

fun fromIndex i = T {index = ref i}

fun compare (opt, opt') = Int.compare (index opt, index opt')

fun equals (opt, opt') = index opt = index opt'

val op <= = fn (opt, opt') => index opt <= index opt'

fun toString (opt: t): string =
   concat ["opt_", Int.toString (index opt)]

val layout = Layout.str o toString

val stack = new ()
val thread = new ()
val weakGone = new ()

local
   val word8Vector = new ()
   val word16Vector = new ()
   val word32Vector = new ()
   val word64Vector = new ()
in
   fun wordVector (b: Bits.t): t =
      case Bits.toInt b of
         8 => word8Vector
       | 16 => word16Vector
       | 32 => word32Vector
       | 64 => word64Vector
       | _ => Error.bug "ObjptrTycon.wordVector"
end

end
