(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type int = Int.t
   
signature CONSOLE =
   sig
      structure Background:
         sig
            datatype t =
               Black | Red | Green | Brown | Blue | Magenta | Cyan | Gray
         end

      structure Foreground:
         sig
            datatype t =
               DarkGray | BrightRed | BrightGreen | Yellow | BrightBlue
             | BrightMagenta | BrightCyan | White
         end
         
      structure CharRendition:
         sig
            datatype t =
               Default (* Normal, UnderlineOff, ReverseVideoOf,f BlinkOff *)
             | Bold
             | Dim
             | Normal
             | UnderlineOn
             | UnderlineOff
             | UnderlineOnDefaultForeground
             | UnderlineOffDefaultForeground
             | BlinkOn
             | BlinkOff
             | ReverseVideoOn
             | ReverseVideoOff
             | Foreground of Foreground.t
             | Background of Background.t

            val set: t list -> string
         end

      val moveToColumn: int -> string
   end
