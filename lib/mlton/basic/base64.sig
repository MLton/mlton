(* Base64 encoding, as in RFC 1421 *)

type int = Int.t
   
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
   ("Base64", fn () =>
    List.forall(["a", "aa", "aaa", "aaaa", "aaaaa", "aaaaaa", "aaaaaaa",
		 "a", "ab", "abc", "abcd", "abcde", "abcdef", "abcdefg",
		 "bb:new.site"],
		fn s => decode(encode s) = s))
end
