(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PrimTycons (S: PRIM_TYCONS_STRUCTS): PRIM_TYCONS =
struct

open S

structure BindingStrength =
   struct
      datatype t =
         Arrow
       | Tuple
       | Unit

      val unit = Unit
   end

datatype z = datatype RealSize.t

type tycon = t

local
   fun make s = (s, fromString s)
in
   val array = make "array"
   val arrow = make "->"
   val bool = make "bool" 
   val cpointer = make "cpointer"
   val exn = make "exn"
   val intInf = make "intInf"
   val list = make "list"
   val reff = make "ref"
   val thread = make "thread"
   val tuple = make "*"
   val vector = make "vector"
   val weak = make "weak"
end

datatype z = datatype Kind.t
datatype z = datatype AdmitsEquality.t

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
            (all, fn s => let
               val name = concat [prefix, Bits.toString (bits s)]
            in
               {name = name,
                size = s,
                tycon = fromString name}
            end)
         val fromSize =
            memo
            (fn s =>
             case Vector.peek (all, fn {size = s', ...} => equalsA (s, s')) of
                NONE => Error.bug "PrimTycons.make.fromSize"
              | SOME {tycon, ...} => tycon)
         fun is t = Vector.exists (all, fn {tycon = t', ...} => equals (t, t'))
         fun de t = 
            case Vector.peek (all, fn {tycon = t', ...} => equals (t, t')) of
               NONE => Error.bug "PrimTycons.make.de"
             | SOME {size, ...} => size
         val prims =
            Vector.toListMap (all, fn {name, tycon, ...} =>
                              {admitsEquality = admitsEquality,
                               kind = Arity 0,
                               name = name,
                               tycon = tycon})
         val all = Vector.map (all, fn {tycon, size, ...} => (tycon, size))
      in
         (fromSize, all, is, de, prims)
      end
in
   val (char, _, isCharX, deCharX, primChars) =
      let
         open CharSize
      in
         make ("char", all, bits, equals, memoize, Sometimes)
      end
   val (int, ints, isIntX, deIntX, primInts) =
      let
         open IntSize
      in
         make ("int", all, bits, equals, memoize, Sometimes)
      end
   val (real, reals, isRealX, deRealX, primReals) =
      let
         open RealSize
      in
         make ("real", all, bits, equals, memoize, Never)
      end
   val (word, words, isWordX, deWordX, primWords) =
      let
         open WordSize
      in
         make ("word", all, bits, equals, memoize, Sometimes)
      end
end

val prims =
   List.map ([(array, Arity 1, Always),
              (arrow, Arity 2, Never),
              (bool, Arity 0, Sometimes),
              (cpointer, Arity 0, Always),
              (exn, Arity 0, Never),
              (intInf, Arity 0, Sometimes),
              (list, Arity 1, Sometimes),
              (reff, Arity 1, Always),
              (thread, Arity 0, Never),
              (tuple, Nary, Sometimes),
              (vector, Arity 1, Sometimes),
              (weak, Arity 1, Never)],
             fn ((name, tycon), kind, admitsEquality) =>
             {admitsEquality = admitsEquality,
              kind = kind,
              name = name,
              tycon = tycon})
   @ primChars @ primInts @ primReals @ primWords

val array = #2 array
val arrow = #2 arrow
val bool = #2 bool
val cpointer = #2 cpointer
val exn = #2 exn
val intInf = #2 intInf
val list = #2 list
val reff = #2 reff
val thread = #2 thread
val tuple = #2 tuple
val vector = #2 vector
val weak = #2 weak

val defaultChar = fn () => 
   case !Control.defaultChar of
      "char8" => char CharSize.C8
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

val isBool = fn c => equals (c, bool)
val isCPointer = fn c => equals (c, cpointer)
val isIntX = fn c => equals (c, intInf) orelse isIntX c
val deIntX = fn c => if equals (c, intInf) then NONE else SOME (deIntX c)

fun layoutApp (c: t,
               args: (Layout.t * ({isChar: bool}
                                  * BindingStrength.t)) vector) =
   let
      local
         open Layout
      in
         val mayAlign = mayAlign
         val seq = seq
         val str = str
      end
      datatype z = datatype BindingStrength.t
      datatype binding_context =
         ArrowLhs
       | ArrowRhs
       | TupleElem
       | Tyseq1
       | TyseqN
      fun maybe bindingContext (l, ({isChar = _}, bindingStrength)) =
         case (bindingStrength, bindingContext) of
            (Unit, _) => l
          | (Tuple, ArrowLhs) => l
          | (Tuple, ArrowRhs) => l
          | (Tuple, TyseqN) => l
          | (Arrow, ArrowRhs) => l
          | (Arrow, TyseqN) =>  l
          | _ => Layout.paren l
      fun normal () =
         let
            val ({isChar}, lay) =
               case Vector.length args of
                  0 => ({isChar = equals (c, defaultChar ())}, layout c)
                | 1 => ({isChar = false},
                        seq [maybe Tyseq1 (Vector.first args),
                             str " ", layout c])
                | _ => ({isChar = false},
                        seq [Layout.tuple
                             (Vector.toListMap (args, maybe TyseqN)),
                             str " ", layout c])
         in
            (lay, ({isChar = isChar}, Unit))
         end
   in
      if equals (c, arrow)
         then (mayAlign [maybe ArrowLhs (Vector.first args),
                         seq [str "-> ",
                              maybe ArrowRhs (Vector.sub (args, 1))]],
               ({isChar = false}, Arrow))
      else if equals (c, tuple)
         then if Vector.isEmpty args
                 then (str "unit", ({isChar = false}, Unit))
              else (mayAlign (Layout.separateLeft
                              (Vector.toListMap (args, maybe TupleElem), "* ")),
                    ({isChar = false}, Tuple))
      else if equals (c, vector)
         then if #isChar (#1 (#2 (Vector.first args)))
                 then (str "string", ({isChar = false}, Unit))
              else normal ()
      else normal ()
   end

fun layoutFormal (c: t, args: (Layout.t * ({isChar: bool}* BindingStrength.t)) vector) =
   let
      local
         open Layout
         open BindingStrength
      in
         val mayAlign = mayAlign
         val seq = seq
         val str = str
      end
      val ({isChar}, lay) =
         case Vector.length args of
            0 => if equals (c, tuple)
                    then (({isChar = false}, str "unit"))
                 else
                    ({isChar = equals (c, defaultChar ())},
                     (* always use canonical names, even if synonyms exist *)
                     if equals (c, real RealSize.R32)
                        then str "real32"
                     else if equals(c, real RealSize.R64)
                        then str "real64"
                     else if equals(c, word WordSize.word8)
                        then str "word8"
                     else if equals(c, word WordSize.word16)
                        then str "word16"
                     else if equals(c, word WordSize.word32)
                        then str "word32"
                     else if equals(c, word WordSize.word64)
                        then str "word64"
                     else if equals(c, intInf)
                        then str "intInf"
                     else if equals(c, bool)
                        then str "bool"
                     else layout c)
          | 1 => ({isChar = false},
                   seq [(Layout.paren o #1) (Vector.sub (args, 0)),
                   str " ", 
                      if equals (c, tuple)
                         then str "tuple"  
                      else if equals(c, vector)
                         then str "vector"
                      else if equals(c, array)
                         then str "array"
                      else if equals(c, reff)
                         then str "ref"
                      else if equals(c, weak)
                         then str "weak"
                         else layout c])
          | _ => ({isChar = false},
                   seq [Layout.tuple
                   (Vector.toListMap (args, #1)),
                    str " ", 
                       if equals (c, tuple)
                          then str "tuple"
                       else if equals (c, arrow)
                         then str "arrow"
                       else layout c])
         in
            (lay, ({isChar = isChar}, BindingStrength.unit))
         end
end
