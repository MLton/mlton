(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure SML90:> SML90 =
   struct
      type instream = TextIO.instream
      type outstream = TextIO.outstream
      exception Abs = Overflow
      exception Quot = Overflow
      exception Prod = Overflow
      exception Neg = Overflow
      exception Sum = Overflow
      exception Diff = Overflow
      exception Floor = Overflow
      exception Exp = Overflow
      exception Sqrt
      exception Ln
      exception Ord
      exception Mod = Div
      exception Io of string 
      exception Interrupt

      local open Real.Math
      in
         val sqrt = fn x => if Real.< (x, 0.0) then raise Sqrt else sqrt x
         val exp = fn x => let val y = exp x
                           in if Real.isFinite y
                                 then y
                              else raise Exp
                           end
         val ln = fn x => if Real.> (x, 0.0) then ln x else raise Ln
         val sin = sin
         val cos = cos
         val arctan = atan
      end

      fun ord s =
         if String.size s = 0
            then raise Ord
         else Char.ord(String.sub(s, 0))

      val chr = String.str o Char.chr
      fun explode s = List.map String.str (String.explode s)
      val implode = String.concat
      fun lookahead ins =
         case TextIO.lookahead ins of
            NONE => ""
          | SOME c => str c

      val std_in = TextIO.stdIn
      fun open_in f =
         TextIO.openIn f handle IO.Io _ => raise Io (concat ["Cannot open ", f])
      fun input ins =
         TextIO.inputN ins handle IO.Io _ => raise Io "Input stream is closed"
      val close_in = TextIO.closeIn
      fun end_of_stream ins = TextIO.endOfStream ins handle _ => true
      val std_out = TextIO.stdOut
      fun open_out f =
         TextIO.openOut f
         handle IO.Io _ => raise Io (concat ["Cannot open ", f]) 
      fun output (out, s) =
         TextIO.output (out, s)
         handle IO.Io _ => raise Io "Output stream is closed"
      val close_out = TextIO.closeOut
   end
