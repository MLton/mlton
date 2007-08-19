(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure CM: CM =
struct

val maxAliasNesting: int = 32

fun cm {cmfile: File.t} =
   let
      val files = ref []
      (* The files in seen are absolute. *)
      val seen = String.memoize (fn _ => ref false)
      fun loop (cmfile: File.t,
                nesting: int,
                relativize: Dir.t option): unit =
         let
            val relativize =
               case relativize of
                  NONE => NONE
                | _ => if OS.Path.isAbsolute cmfile
                          then NONE
                       else relativize
            val {dir, file} = OS.Path.splitDirFile cmfile
         in
            Dir.inDir
            (if dir = "" then "." else dir, fn () =>
                let
                   val cwd = Dir.current ()
                   fun abs f = OS.Path.mkAbsolute {path = f, relativeTo = cwd}
                   fun finalize f =
                      case relativize of
                         NONE => abs f
                       | SOME d =>
                            OS.Path.mkRelative {path = f,
                                                relativeTo = d}
                   fun region () =
                      let
                         val sourcePos =
                            SourcePos.make {column = 0,
                                            file = finalize cmfile,
                                            line = 0}
                      in
                         Region.make {left = sourcePos, right = sourcePos}
                      end
                   fun fail msg =
                      Control.error (region (), Layout.str msg, Layout.empty)
                   datatype z = datatype Parse.result
                in
                   case Parse.parse {cmfile = file} of
                      Alias f =>
                         if nesting > maxAliasNesting
                            then fail "alias nesting too deep."
                         else loop (f, nesting + 1, relativize)
                    | Bad s => fail (concat ["bad CM file: ", s])
                    | Members members =>
                         List.foreach
                         (members, fn m =>
                          let
                             val m' = abs m
                             val seen = seen m'
                          in
                             if !seen
                                then ()
                             else let
                                     val _ = seen := true
                                     fun sml () =
                                        List.push (files, finalize m')
                                  in
                                     Control.checkFile
                                     (m,
                                      {fail = fail,
                                       name = m,
                                       ok = fn () =>
                                       case File.suffix m of
                                          SOME "cm" =>
                                             loop (m, 0, relativize)
                                        | SOME "sml" => sml ()
                                        | SOME "sig" => sml ()
                                        | SOME "fun" => sml ()
                                        | SOME "ML" => sml ()
                                        | _ =>
                                             fail (concat ["MLton can't process ",
                                                           m])})
                                  end
                          end)
                end)
         end 
      val d = Dir.current ()
      val _ = loop (cmfile, 0, SOME d)
      val files = rev (!files)
   in
      files
   end

end
