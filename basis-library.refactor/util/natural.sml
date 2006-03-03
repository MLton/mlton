(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Natural =
   struct
      fun foldStartStop (start, stop, b, f) =
         if start > stop
            then raise Subscript
         else 
            let
               fun loop (i, b) =
                  if i >= stop then b
                  else loop (i + 1, f (i, b))
            in loop (start, b)
            end

      fun foreachStartStop (start, stop, f) =
         foldStartStop (start, stop, (), fn (i, ()) => f i)

      fun foreach (n, f) = foreachStartStop (0, n, f)
   end
