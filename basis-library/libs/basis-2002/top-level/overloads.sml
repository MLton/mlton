(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

(*
    * int  = {Int.int, Int8.int, Int16.int, Int32.int, Int64.int, 
              IntInf.int, LargeInt.int, FixedInt.int, Position.int}
    * word = {Word.word, Word8.word, Word16.word, Word32.word, 
              LargeWord.word, SysWord.word}
    * real = {Real.real, Real32.real, Real.64.real,
              LargeReal.real}
    * text = {String.string, Char.char}
    * wordint = word union int
    * realint = real union int
    * num = word union int union real
    * numtext = num union text 

num ===
_overload f : ?
as  Int.f
and Int8.f
and Int16.f
and Int32.f
(* and Int64.f *)
and IntInf.f
and LargeInt.f
and FixedInt.f
and Position.f
and Word.f
and Word8.f
and Word16.f
and Word32.f
and LargeWord.f
and SysWord.f
and Real.f
and Real32.f
and Real64.f
and LargeReal.f

wordint ===
_overload f : ?
as  Int.f
and Int8.f
and Int16.f
and Int32.f
(* and Int64.f *)
and IntInf.f
and LargeInt.f
and FixedInt.f
and Position.f
and Word.f
and Word8.f
and Word16.f
and Word32.f
and LargeWord.f
and SysWord.f

realint ===
_overload f : ?
as  Int.f
and Int8.f
and Int16.f
and Int32.f
(* and Int64.f *)
and IntInf.f
and LargeInt.f
and FixedInt.f
and Position.f
and Real.f
and Real32.f
and Real64.f
and LargeReal.f

numtext ===
_overload f : ?
as  Int.f
and Int8.f
and Int16.f
and Int32.f
(* and Int64.f *)
and IntInf.f
and LargeInt.f
and FixedInt.f
and Position.f
and Word.f
and Word8.f
and Word16.f
and Word32.f
and LargeWord.f
and SysWord.f
and Real.f
and Real32.f
and Real64.f
and LargeReal.f
and String.f
and Char.f

*)

_overload ~ :   ('a -> 'a) (* num -> num *)
as  Int.~
and Int8.~
and Int16.~
and Int32.~
(* and Int64.~ *)
and IntInf.~
and LargeInt.~
and FixedInt.~
and Position.~
and Word.~
and Word8.~
and Word16.~
and Word32.~
and LargeWord.~
and SysWord.~
and Real.~
and Real32.~
and Real64.~
and LargeReal.~

_overload + :   ('a * 'a -> 'a) (* num * num -> num *)
as  Int.+
and Int8.+
and Int16.+
and Int32.+
(* and Int64.+ *)
and IntInf.+
and LargeInt.+
and FixedInt.+
and Position.+
and Word.+
and Word8.+
and Word16.+
and Word32.+
and LargeWord.+
and SysWord.+
and Real.+
and Real32.+
and Real64.+
and LargeReal.+

_overload - :   ('a * 'a -> 'a) (* num * num -> num *)
as  Int.-
and Int8.-
and Int16.-
and Int32.-
(* and Int64.- *)
and IntInf.-
and LargeInt.-
and FixedInt.-
and Position.-
and Word.-
and Word8.-
and Word16.-
and Word32.-
and LargeWord.-
and SysWord.-
and Real.-
and Real32.-
and Real64.-
and LargeReal.-

_overload * :   ('a * 'a -> 'a) (* num * num -> num *)
as  Int.*
and Int8.*
and Int16.*
and Int32.*
(* and Int64.* *)
and IntInf.*
and LargeInt.*
and FixedInt.*
and Position.*
and Word.*
and Word8.*
and Word16.*
and Word32.*
and LargeWord.*
and SysWord.*
and Real.*
and Real32.*
and Real64.*
and LargeReal.*

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

_overload div:   ('a * 'a -> 'a) (* wordint * wordint -> wordint *)
as  Int.div
and Int8.div
and Int16.div
and Int32.div
(* and Int64.div *)
and IntInf.div
and LargeInt.div
and FixedInt.div
and Position.div
and Word.div
and Word8.div
and Word16.div
and Word32.div
and LargeWord.div
and SysWord.div

_overload mod:   ('a * 'a -> 'a) (* wordint * wordint -> wordint *)
as  Int.mod
and Int8.mod
and Int16.mod
and Int32.mod
(* and Int64.mod *)
and IntInf.mod
and LargeInt.mod
and FixedInt.mod
and Position.mod
and Word.mod
and Word8.mod
and Word16.mod
and Word32.mod
and LargeWord.mod
and SysWord.mod

_overload abs:   ('a -> 'a) (* realint * realint -> realint *)
as  Int.abs
and Int8.abs
and Int16.abs
and Int32.abs
(* and Int64.abs *)
and IntInf.abs
and LargeInt.abs
and FixedInt.abs
and Position.abs
and Real.abs
and Real32.abs
and Real64.abs
and LargeReal.abs

_overload < :   ('a * 'a -> bool) (* numtext * numtext -> bool *)
as  Int.<
and Int8.<
and Int16.<
and Int32.<
(* and Int64.< *)
and IntInf.<
and LargeInt.<
and FixedInt.<
and Position.<
and Word.<
and Word8.<
and Word16.<
and Word32.<
and LargeWord.<
and SysWord.<
and Real.<
and Real32.<
and Real64.<
and LargeReal.<
and String.<
and Char.<

_overload <= :   ('a * 'a -> bool) (* numtext * numtext -> bool *)
as  Int.<=
and Int8.<=
and Int16.<=
and Int32.<=
(* and Int64.<= *)
and IntInf.<=
and LargeInt.<=
and FixedInt.<=
and Position.<=
and Word.<=
and Word8.<=
and Word16.<=
and Word32.<=
and LargeWord.<=
and SysWord.<=
and Real.<=
and Real32.<=
and Real64.<=
and LargeReal.<=
and String.<=
and Char.<=

_overload > :   ('a * 'a -> bool) (* numtext * numtext -> bool *)
as  Int.>
and Int8.>
and Int16.>
and Int32.>
(* and Int64.> *)
and IntInf.>
and LargeInt.>
and FixedInt.>
and Position.>
and Word.>
and Word8.>
and Word16.>
and Word32.>
and LargeWord.>
and SysWord.>
and Real.>
and Real32.>
and Real64.>
and LargeReal.>
and String.>
and Char.>

_overload >= :   ('a * 'a -> bool) (* numtext * numtext -> bool *)
as  Int.>=
and Int8.>=
and Int16.>=
and Int32.>=
(* and Int64.>= *)
and IntInf.>=
and LargeInt.>=
and FixedInt.>=
and Position.>=
and Word.>=
and Word8.>=
and Word16.>=
and Word32.>=
and LargeWord.>=
and SysWord.>=
and Real.>=
and Real32.>=
and Real64.>=
and LargeReal.>=
and String.>=
and Char.>=


