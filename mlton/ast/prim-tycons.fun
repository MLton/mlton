(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
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
val reff = fromString "ref"
val thread = fromString "thread"
val tuple = fromString "*"
val vector = fromString "vector"
val weak = fromString "weak"

datatype z = datatype Kind.t
datatype z = datatype AdmitsEquality.t

val isBool = fn c => equals (c, bool)
val isExn = fn c => equals (c, exn)

local
   fun 'a make (prefix: string,
                all: 'a list,
                bits: 'a -> Bits.t,
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
                NONE => Error.bug "PrimTycons.make"
              | SOME (tycon, _) => tycon)
         fun is t = Vector.exists (all, fn (t', _) => equals (t, t'))
         val prims =
            Vector.toListMap (all, fn (tycon, _) =>
                              (tycon, Arity 0, admitsEquality))
      in
         (fromSize, all, is, prims)
      end
in
   val (char, _, isCharX, primChars) =
      let
         open CharSize
      in
         make ("char", all, bits, equals, memoize, Sometimes)
      end
   val (int, ints, isIntX, primInts) =
      let
         open IntSize
      in
         make ("int", all, bits, equals, memoize, Sometimes)
      end
   val (real, reals, isRealX, primReals) =
      let
         open RealSize
      in
         make ("real", all, bits, equals, memoize, Never)
      end
   val (word, words, isWordX, primWords) =
      let
         open WordSize
      in
         make ("word", all, bits, equals, memoize, Sometimes)
      end
end

val defaultChar = fn () => 
   case !Control.defaultChar of
      "char8" => char CharSize.C1
    | _ => Error.bug "PrimTycons.defaultChar"
val defaultInt = fn () => 
   case !Control.defaultInt of
      "int8" => int (IntSize.fromBits (Bits.fromInt 8))
    | "int16" => int (IntSize.fromBits (Bits.fromInt 16))
    | "int32" => int (IntSize.fromBits (Bits.fromInt 32))
    | "int64" => int (IntSize.fromBits (Bits.fromInt 64))
    | "intinf" => intInf
    | _ => Error.bug "PrimTycons.defaultInt"
val defaultReal = fn () => 
   case !Control.defaultReal of
      "real32" => real RealSize.R32
    | "real64" => real RealSize.R64
    | _ => Error.bug "PrimTycons.defaultReal"
val defaultWord = fn () => 
   case !Control.defaultWord of
      "word8" => word (WordSize.fromBits (Bits.fromInt 8))
    | "word16" => word (WordSize.fromBits (Bits.fromInt 16))
    | "word32" => word (WordSize.fromBits (Bits.fromInt 32))
    | "word64" => word (WordSize.fromBits (Bits.fromInt 64))
    | _ => Error.bug "PrimTycons.defaultWord"

val isIntX = fn c => equals (c, intInf) orelse isIntX c

val prims =
   [(array, Arity 1, Always),
    (arrow, Arity 2, Never),
    (bool, Arity 0, Sometimes),
    (exn, Arity 0, Never),
    (intInf, Arity 0, Sometimes),
    (list, Arity 1, Sometimes),
    (pointer, Arity 0, Always),
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
                  0 => ({isChar = equals (c, defaultChar ())}, layout c)
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
          
