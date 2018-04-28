(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure System: SYSTEM =
   struct
      fun insertBackslashes (ss: string list,
                             width: int,
                             indent: int): string list =
         let
            val indentation = String.make (indent, #" ")
            fun loop (ss, pos, line, lines) =
               (* pos + 2 < width (so the backslash can be inserted) *)
               case ss of
                  [] => rev (concat (rev line) :: lines)
                | s :: ss =>
                     let
                        val n = String.size s
                        val (pos, line') =
                           case line of
                              [] => (pos + n, [s])
                            | _ => (pos + n + 1, s :: " " :: line)
                        fun newLine () =
                           loop (ss, indent + n, [s, indentation],
                                 concat (rev (" \\" :: line)) :: lines)
                     in
                        if pos <= width
                           then
                              case ss of
                                 [] => rev (concat (rev line') :: lines)
                               | _ => 
                                    if pos + 2 <= width
                                       then loop (ss, pos, line', lines)
                                    else newLine ()
                        else newLine ()
                     end
         in loop (ss, 0, [], [])
         end

      fun system (com: string, args: string list): unit =
         let
            (* Many terminal emulators do the line folding one character early,
             * so we use 79 instead of 80 columns.
             *)
            val width = 79
            val indentAmount = 4
            val s = concat (List.separate (com :: args, " "))
            val _ =
               let
                  open Control
               in
                  message (Top, fn () =>
                           Layout.align
                           (List.map (insertBackslashes
                                      (com :: args,
                                       width - getDepth (),
                                       indentAmount),
                                      Layout.str)))
               end
         in
            Process.wait (MLton.Process.spawnp {file = com, args = com :: args})
            handle e => Error.bug (concat ["call to system failed with ",
                                           Exn.toString e, ":\n", s])
         end
   end
