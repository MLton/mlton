(*
 * endian-little.sml - How to get at a bit field on a "little endian" machine.
 *
 *  (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure EndianLittle = struct
    fun shift (s: int, ib: int, b: word) = Word.fromInt (ib - s) - b
end
