(*
 * endian-big.sml - How to get at a bit field on a "big endian" machine.
 *
 *  (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure EndianBig = struct
    fun shift (s: int, ib: int, b: word) = Word.fromInt s
end
