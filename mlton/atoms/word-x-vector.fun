(* Copyright (C) 2014 Matthew Fluet.
 * Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor WordXVector (S: WORD_X_VECTOR_STRUCTS): WORD_X_VECTOR =
struct

open S

datatype t = T of {elementSize: WordSize.t,
                   elements: WordX.t vector}

local
   fun make f (T r) = f r
in
   val elementSize = make #elementSize
   val elements = make #elements
end

fun layout (T {elements, elementSize}) =
   let
      fun vector () =
         Layout.seq
         [Layout.str "#[",
          Layout.fill (Layout.separateRight
                       (Vector.toListMap
                        (elements, WordX.layout),
                        ",")),
          Layout.str "]"]
      fun string cs =
         Layout.seq
         [Layout.str "\"",
          Layout.str (String.escapeSML (String.implodeV cs)),
          Layout.str "\""]
   in
      if WordSize.equals (elementSize, WordSize.word8)
         then let
                 val cs = Vector.map (elements, WordX.toChar)
                 val l = Vector.length cs
                 val n = Vector.fold (cs, 0, fn (c, n) =>
                                      if Char.isGraph c
                                         orelse Char.isSpace c
                                         then n + 1
                                         else n)
              in
                 if l = 0 orelse (10 * n) div l > 9
                    then string cs
                    else vector ()
              end
         else vector ()
   end

val toString = Layout.toString o layout

val hash = String.hash o toString

fun equals (v, v') =
    WordSize.equals (elementSize v, elementSize v')
    andalso Vector.equals (elements v, elements v', WordX.equals)

fun foldFrom (v, start, b, f) = Vector.foldFrom (elements v, start, b, f)

fun forall (v, f) = Vector.forall (elements v, f)

fun fromListRev ({elementSize}, l) =
   T {elementSize = elementSize,
      elements = Vector.fromListRev l}

fun fromString s =
   T {elementSize = WordSize.byte,
      elements = Vector.tabulate (String.size s, fn i =>
                                  WordX.fromChar (String.sub (s, i)))}

fun length v = Vector.length (elements v)

fun sub (v, i) = Vector.sub (elements v, i)

fun tabulate ({elementSize}, n, f) =
   T {elementSize = elementSize,
      elements = Vector.tabulate (n, f)}

fun toListMap (v, f) = Vector.toListMap (elements v, f)

end
