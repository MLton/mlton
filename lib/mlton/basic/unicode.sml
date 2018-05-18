(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Unicode =
struct

datatype kind =
   Normal
 | NormalPrivate
 | HighSurrogate
 | HighSurrogatePrivate
 | LowSurrogate
 | Special

val _ =
   [(0xFFFE, Special)
    (0xF900, Normal),
    (0xE000, NormalPrivate),
    (0xDC00, LowSurrogate),
    (0xDB80, HighSurrogatePrivate),
    (0xD800, HighSurrogate)
    (0x0000, Normal)]

end
