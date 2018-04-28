(* Copyright (C) 2011,2015 Matthew Fluet.
 * Copyright (C) 1999-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Source: SOURCE =
struct

datatype t = T of {file: File.t ref,
                   lineNum: int ref,
                   lineStart: int ref,
                   origDir: Dir.t}

fun getPos (T {file, lineNum, lineStart, ...}, n) =
   SourcePos.make {column = n - !lineStart,
                   file = !file,
                   line = !lineNum}

fun lineStart (s as T {lineStart, ...}) = getPos (s, !lineStart)

fun lineDirective (T {file, lineNum, lineStart, origDir},
                   f,
                   {lineNum = n, lineStart = s}) =
   (Option.app (f, fn f =>
                let
                   val f =
                      if OS.Path.isAbsolute f
                         then f
                      else OS.Path.mkCanonical (OS.Path.concat (origDir, f))
                in
                   file := f
                end)
    ; lineNum := n
    ; lineStart := s)

fun new file = T {file = ref file,
                  lineNum = ref 1,
                  (* mllex file positions start at zero, while we report errors
                   * starting in column 1, so we need to pretend the first line
                   * starts at position ~1, which will translate position 0 to
                   * column 1.
                   *)
                  lineStart = ref ~1,
                  origDir = File.dirOf file}

fun newline (T {lineStart, lineNum, ...}, n) =
   (Int.inc lineNum
    ; lineStart := n)

fun name (T {file, ...}) = !file

end
