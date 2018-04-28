(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure CharBuffer: CHAR_BUFFER = 
struct

datatype t = T of {length: int ref,
                   chars: char array ref}

val initChar = #"\013"

fun new () = T {length = ref 0,
              chars = ref (Array.array (1, initChar))}

fun length (T {length, ...}) = !length

fun reset (T {length, ...}) = length := 0

val growFactor: int = 2

structure Int =
   struct
      open Int
      val max = Trace.trace2 ("CharBuffer.Int.max", layout, layout, layout) max
   end

fun ensureFree (T {length, chars, ...}, amount: int): unit =
   let val maxLength = Array.length (!chars)
   in if amount <= maxLength - !length
         then ()
      else
         let val n = Int.max (maxLength * growFactor, !length + amount)
            val a = !chars
         in chars := Array.tabulate (n, fn i =>
                                    if i < maxLength
                                       then Array.sub (a, i)
                                    else initChar)
         end
   end

fun addChar (v as T {length, chars, ...}, c) =
   (ensureFree (v, 1)
    ; Array.update (!chars, !length, c)
    ; Int.inc length)

fun toString (T {length, chars, ...}): string =
   let val a = !chars
   in CharVector.tabulate (!length, fn i => Array.sub (a, i))
   end

val layout = Layout.str o toString

val addChar =
   Trace.trace2 ("CharBuffer.addChar", layout, Char.layout, Unit.layout) addChar

end
