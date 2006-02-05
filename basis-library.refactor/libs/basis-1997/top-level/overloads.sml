(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

_overload ~ :   ('a -> 'a)
as  Int.~
and IntInf.~
and Real.~

_overload + :   ('a * 'a -> 'a)
as  Int.+
and IntInf.+
and Word.+
and Word8.+
and Real.+

_overload - :   ('a * 'a -> 'a)
as  Int.-
and IntInf.-
and Word.-
and Word8.-
and Real.-

_overload * :   ('a * 'a -> 'a)
as  Int.*
and IntInf.*
and Word.*
and Word8.*
and Real.*

(* Can't use the following overload, because then
 *   fun f (x, y) = x + y / y
 * fails to type check.  The problem is that because + and / are not constrained,
 * the type checker chooses the default type for +, int * int -> int.  It is
 * then screwed because it can't chose that type for /.  The problem happens
 * when there are overloaded variables that have some compatible type (in this
 * case real) but one of whose default types (int) is not a valid instance
 * of the other.
 *)
(*
 * _overload / : ('a * 'a -> 'a)
 * as Real./
 *)
val op / = Real./ 

_overload div: ('a * 'a -> 'a)
as  Int.div
and IntInf.div
and Word.div
and Word8.div

_overload mod: ('a * 'a -> 'a)
as  Int.mod
and IntInf.mod
and Word.mod
and Word8.mod

_overload < :   ('a * 'a -> bool)
as  Int.<
and IntInf.<
and Word.<
and Word8.<
and Real.<
and Char.<
and String.<

_overload <= :   ('a * 'a -> bool)
as  Int.<=
and IntInf.<=
and Word.<=
and Word8.<=
and Real.<=
and Char.<=
and String.<=

_overload > :   ('a * 'a -> bool)
as  Int.>
and IntInf.>
and Word.>
and Word8.>
and Real.>
and Char.>
and String.>

_overload >= :   ('a * 'a -> bool)
as  Int.>=
and IntInf.>=
and Word.>=
and Word8.>=
and Real.>=
and Char.>=
and String.>=

_overload abs: ('a -> 'a)
as  Int.abs
and IntInf.abs
and Real.abs

