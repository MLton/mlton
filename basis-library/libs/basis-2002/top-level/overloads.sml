(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

_overload ~ :   ('a -> 'a)
as  Int.~
(* and Int64.~ *)
and Int32.~
and Int16.~
and Int8.~
and IntInf.~
and Position.~
and Word.~
and Word32.~
and Word16.~
and Word8.~
and SysWord.~
and Real.~
and Real64.~
and Real32.~

_overload + :   ('a * 'a -> 'a)
as  Int.+
(* and Int64.+ *)
and Int32.+
and Int16.+
and Int8.+
and IntInf.+
and Position.+
and Word.+
and Word32.+
and Word16.+
and Word8.+
and SysWord.+
and Real.+
and Real64.+
and Real32.+

_overload - :   ('a * 'a -> 'a)
as  Int.-
(* and Int64.- *)
and Int32.-
and Int16.-
and Int8.-
and IntInf.-
and Position.-
and Word.-
and Word32.-
and Word16.-
and Word8.-
and SysWord.-
and Real.-
and Real64.-
and Real32.-

_overload * :   ('a * 'a -> 'a)
as  Int.*
(* and Int64.* *)
and Int32.*
and Int16.*
and Int8.*
and IntInf.*
and Position.*
and Word.*
and Word32.*
and Word16.*
and Word8.*
and SysWord.*
and Real.*
and Real64.*
and Real32.*

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
(* and Int64.div *)
and Int32.div
and Int16.div
and Int8.div
and IntInf.div
and Position.div
and Word.div
and Word32.div
and Word16.div
and Word8.div
and SysWord.div

_overload mod: ('a * 'a -> 'a)
as  Int.mod
(* and Int64.mod *)
and Int32.mod
and Int16.mod
and Int8.mod
and IntInf.mod
and Position.mod
and Word.mod
and Word32.mod
and Word16.mod
and Word8.mod
and SysWord.mod

_overload < :   ('a * 'a -> bool)
as  Int.<
(* and Int64.< *)
and Int32.<
and Int16.<
and Int8.<
and IntInf.<
and Position.<
and Word.<
and Word32.<
and Word16.<
and Word8.<
and SysWord.<
and Real.<
and Real64.<
and Real32.<
and String.<
and Char.<

_overload <= :   ('a * 'a -> bool)
as  Int.<=
(* and Int64.<= *)
and Int32.<=
and Int16.<=
and Int8.<=
and IntInf.<=
and Position.<=
and Word.<=
and Word32.<=
and Word16.<=
and Word8.<=
and SysWord.<=
and Real.<=
and Real64.<=
and Real32.<=
and String.<=
and Char.<=

_overload > :   ('a * 'a -> bool)
as  Int.>
(* and Int64.> *)
and Int32.>
and Int16.>
and Int8.>
and IntInf.>
and Position.>
and Word.>
and Word32.>
and Word16.>
and Word8.>
and SysWord.>
and Real.>
and Real64.>
and Real32.>
and String.>
and Char.>

_overload >= :   ('a * 'a -> bool)
as  Int.>=
(* and Int64.>= *)
and Int32.>=
and Int16.>=
and Int8.>=
and IntInf.>=
and Position.>=
and Word.>=
and Word32.>=
and Word16.>=
and Word8.>=
and SysWord.>=
and Real.>=
and Real64.>=
and Real32.>=
and String.>=
and Char.>=

_overload abs: ('a -> 'a)
as  Int.abs
(* and Int64.abs *)
and Int32.abs
and Int16.abs
and Int8.abs
and IntInf.abs
and Position.abs
and Real.abs
and Real64.abs
and Real32.abs

