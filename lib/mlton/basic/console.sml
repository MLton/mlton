(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Console: CONSOLE =
struct

(* Information from Chapter 20 of Linux Application Development,
 * by Johnson and Troan.
 *)

structure Background =
   struct
      datatype t = Black | Red | Green | Brown | Blue | Magenta | Cyan | Gray
   end

structure Foreground =
   struct
      datatype t =
         DarkGray | BrightRed | BrightGreen | Yellow | BrightBlue
       | BrightMagenta | BrightCyan | White
   end

val esc = "\027["

structure CharRendition =
   struct
      datatype t =
         Default
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

      fun set(l: t list): string =
         concat(esc
                :: List.fold(rev l, [], fn (c, l) =>
                             let
                                val n =
                                   case c of
                                      Default => "0"
                                    | Bold => "1"
                                    | Dim => "2"
                                    | Normal => "21"
                                    | UnderlineOn => "4"
                                    | UnderlineOff => "24"
                                    | UnderlineOnDefaultForeground => "38"
                                    | UnderlineOffDefaultForeground => "39"
                                    | BlinkOn => "5"
                                    | BlinkOff => "25"
                                    | ReverseVideoOn => "7"
                                    | ReverseVideoOff => "27"
                                    | Foreground f =>
                                         let datatype z = datatype Foreground.t
                                         in case f of
                                            DarkGray => "30"
                                          | BrightRed => "31"
                                          | BrightGreen => "32"
                                          | Yellow => "33"
                                          | BrightBlue => "34"
                                          | BrightMagenta => "35"
                                          | BrightCyan => "36"
                                          | White => "37"
                                         end
                                    | Background b =>
                                         let datatype z = datatype Background.t
                                         in case b of
                                            Black => "40"
                                          | Red => "41"
                                          | Green => "42"
                                          | Brown => "43"
                                          | Blue => "44"
                                          | Magenta => "45"
                                          | Cyan => "46"
                                          | Gray => "47"
                                         end
                             in case l of
                                [] => [n, "m"]
                              | _ => n :: ";" :: l
                             end))
   end

fun moveToColumn c =
   let
      val columns =
         case Process.getEnv "COLUMNS" of
            NONE => 80
          | SOME c => valOf(Int.fromString c)
      (* 300 is kind of arbitrary, but it's what they do in
       * /etc/sysconfig/init.
       *)
   in concat[esc, "300C", esc, Int.toString(columns - c), "D"]
   end

end
