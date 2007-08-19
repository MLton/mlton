(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure SourcePos: SOURCE_POS =
struct

datatype t = T of {column: int,
                   file: File.t,
                   line: int}

local
   fun f g (T r) = g r
in
   val column = f #column
   val line = f #line
end

fun compare (T {column = c, file = f, line = l},
             T {column = c', file = f', line = l'}) =
   case String.compare (f, f') of
      EQUAL =>
         (case Int.compare (l, l') of
             EQUAL => Int.compare (c, c')
           | r => r)
    | r => r

fun equals (T r, T r') = r = r'
val _ = equals

fun make {column, file, line} =
   T {column = column,
      file = file,
      line = line}

fun getLib (T {file, ...}) =
   let 
      val libDir = concat [!ControlFlags.libDir, "/sml"]
   in 
      if String.hasPrefix (file, {prefix = libDir})
         then SOME (String.size libDir)
         else NONE
   end

fun file (p as T {file, ...}) =
   if !ControlFlags.preferAbsPaths
      then file
      else
         case getLib p of
            NONE => file
          | SOME i =>
               concat ["$(SML_LIB)", String.dropPrefix (file, i)]

val bogus = T {column = ~1,
               file = "<bogus>",
               line = ~1}

fun toString (p as T {column, line, ...}) =
   concat [file p, " ", Int.toString line, ".", Int.toString column]

fun posToString (T {line, column, ...}) =
   concat [Int.toString line, ".", Int.toString column]

end
