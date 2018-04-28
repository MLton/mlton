(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

(* Base64 encoding, as in RFC 1421 *)

signature BASE64 =
   sig
      val decode: string -> string
      val encode: string -> string
   end


functor TestBase64 (S: BASE64): sig end =
struct

open S

val _ =
   Assert.assert
   ("TestBase64", fn () =>
    List.forall(["a", "aa", "aaa", "aaaa", "aaaaa", "aaaaaa", "aaaaaaa",
                 "a", "ab", "abc", "abcd", "abcde", "abcdef", "abcdefg",
                 "bb:new.site"],
                fn s => decode(encode s) = s))
end
