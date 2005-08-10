(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
