(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature REGEXP_STRUCTS = 
   sig
   end

signature REGEXP = 
   sig
      include REGEXP_STRUCTS

      structure Save:
         sig
            type t

            val new: unit -> t
         end

      structure Match:
         sig
            type t

            val all: t -> Substring.t
            val startLength: t -> {start: int, length: int}
            val exists: t * Save.t -> bool
            val funs: t -> {exists: Save.t -> bool,
                            lookup: Save.t -> Substring.t,
                            peek: Save.t -> Substring.t option}
            val length: t -> int
            val lookup: t * Save.t -> Substring.t
            val lookupString: t * Save.t -> String.t
            val peek: t * Save.t -> Substring.t option
            val peekString: t * Save.t -> String.t option
            val stringFuns: t -> {exists: Save.t -> bool,
                                  lookup: Save.t -> String.t,
                                  peek: Save.t -> String.t option}
         end

      structure Compiled:
         sig
            type t

            (* Find the first substring of s starting at or after index i that
             * matches r.  Return the first character and the character just
             * past the end of the substring.
             *)
            val findShort: t * string * int -> Match.t option
            val findLong: t * string * int -> Match.t option
            val foreachMatchShort: t * string * (Match.t -> unit) -> unit
            val layout: t -> Layout.t
            val layoutDot: t -> Layout.t
            val layoutDotToFile: t * File.t -> unit
            (* match (r, s, i)
             * Return the (shortest or longest) substring of s starting at index
             * i that matches r. 
             * The substring is represented by the index of the character just
             * past its end.
             * Return NONE if there is NO match.
             * All of the saves in the match will be set.
             *)
            val matchAll: t * string -> Match.t option
            val matchLong: t * string * int -> Match.t option
            val matchShort: t * string * int -> Match.t option
            val matchesAll: t * string -> bool
            val matchesPrefix: t * string -> bool
         end

      type t

      val anchorFinish: t
      val anchorStart: t
      val any: t (* arbitrary character *)
      val anys: t (* arbitrary number of characters *)
      val ascii: t (* arbitrary ascii character *)
      val asciis: t (* arbitrary ascii characters *)
      val char: char -> t
      val compileDFA: t -> Compiled.t
      val compileNFA: t -> Compiled.t
      val digit: t
      val digits: t
      val dquote: t (* char #"\"" *)
      val fromString: string -> (t * Save.t vector) option
      val isChar: (char -> bool) -> t
      val isNotChar: (char -> bool) -> t
      val layout: t -> Layout.t
      val nonDigit: t
      val none: t
      val notChar: char -> t
      val notOneOf: string -> t
      val null: t  (* empty string *)
      val oneOf: string -> t
      val oneOrMore: t -> t
      val optional: t -> t
      val or: t list -> t
      val save: t * Save.t -> t
      val seq: t list -> t
      val spaces: t (* star (isChar Char.isSpace) *)
      val star: t -> t
      val string: string -> t (* case matters *)
      val stringIgnoreCase: string -> t (* case doesn't matter *)
      val toString: t -> string
      val zeroOrMore: t -> t  (* same as star *)
   end


functor TestRegexp (S: REGEXP): sig end =
struct

val _ = print "TestRegexp\n"

open S
open Compiled
val compile = if true then compileNFA else compileDFA

val _ =
   Assert.assert
   ("TestRegexp.save", fn () =>
    let
       val s = Save.new ()
    in
       List.forall
       ([(save (seq [], s), "", ""),
         (save (star (oneOf "a"), s), "", ""),
         (seq [save (seq [], s), seq []], "", ""),
         (seq [oneOf "a", save (seq [], s)], "a", "")],
        fn (r, s1, s2) =>
        let
           val c = compile r
        in
           case matchAll (c, s1) of
              NONE => false
            | SOME m => Match.lookupString (m, s) = s2
        end)
    end)

val _ =
   Assert.assert
   ("TestRegexp.doesMatchAll", fn () =>
    List.forall ([(any, "a"),
                  (anys, "abc")], 
                 fn (r, s) => matchesAll (compile r, s)))
val tests =
   List.map ([
              ("\\a", "a"),
              ("^$", ""),
              ("abc", "abc"),
              (".", "a"),
              ("^foo$", "foo"),
              ("^...$", "foo"),
              ("^.*$", "foo"),
              ("^.*foo@bar\\.com$", "foo@bar.com"),
              ("(abc)","abc"),
              ("\\(abc\\)","(abc)"),
              ("(abc){2,4}$", "abcabc"),
              ("(abc){2,4}$", "abcabcabc"),
              ("(abc){2,4}$", "abcabcabcabc")
              ],
             fn (r, s) =>
             let
                val opt = SOME (String.size s)
             in
                (#1 (valOf (fromString r)), s, opt, opt)
             end)
   @
   [
    (#1 (valOf (fromString "a")), "a", SOME 1, SOME 1),
    (#1 (valOf (fromString "a*")), "a", SOME 0, SOME 1),
    (#1 (valOf (fromString "a+")), "a", SOME 1, SOME 1),
    (#1 (valOf (fromString "a+")), "aa", SOME 1, SOME 2),
    (#1 (valOf (fromString "[^a]")), "a", NONE, NONE),
    (#1 (valOf (fromString "[^a]")), "b", SOME 1, SOME 1),
    (stringIgnoreCase "abc", "abc", SOME (3: int), SOME (3: int)),
    (stringIgnoreCase "abc", "aBC", SOME 3, SOME 3),
    (stringIgnoreCase "ab", "abab", SOME 2, SOME 2),
    (string "abc", "abc", SOME 3, SOME 3),
    (string "Abc", "abc", NONE, NONE),
    (seq [anchorStart, anchorFinish], "", SOME 0, SOME 0),
    (seq [anchorStart, string "abc", anchorFinish], "abc", SOME 3, SOME 3),
    (seq [or [null, anchorFinish], string "a"], "a", SOME 1, SOME 1),
    (seq [or [anchorFinish, null], string "a"], "a", SOME 1, SOME 1),
    (seq [], "abc", SOME 0, SOME 0),
    (seq [string "ab"], "ab", SOME 2, SOME 2),
    (seq [char #"a", char #"b", char #"c"], "abc", SOME 3, SOME 3),
    (seq [string "ab", null], "abc", SOME 2, SOME 2),
    (or [string "a", string "ab", string "abc"], "abc", SOME 1, SOME 3),
    (seq [or [string "ab", null],
         or [string "abcde", string "cd"]], "abcde",
     SOME 4, SOME 5),
    (star (or [null, char #"a"]), "aaa", SOME 0, SOME 3),
    (star (string "ab"), "ababab", SOME 0, SOME 6),
    let val r = Save.new ()
    in (save (string "ab", r), "ab", SOME 2, SOME 2)
    end,
    let val r = Save.new ()
    in (seq [string "a", save (string "bc", r), string "d"],
        "abcd", SOME 4, SOME 4)
    end,
    let val s1 = Save.new ()
       val s2 = Save.new ()
    in (seq [save (string "a", s1),
            save (string "b", s2)],
        "ab", SOME 2, SOME 2)
    end,
 let val s1 = Save.new ()
    val s2 = Save.new ()
 in (seq [save (string "a", s1),
          string "b",
          save (string "c", s2),
          string "d"],
     "abcd",
     SOME 4, SOME 4)
 end,
let val s1 = Save.new ()
in (seq [string "a",
        save (string "b", s1),
        string "c"],
    "abc",
    SOME 3, SOME 3)
end,
let val s1 = Save.new ()
in (seq [string "abc",
        save (string "d", s1),
        string "e"],
    "abcde",
    SOME 5, SOME 5)
end,
let val s1 = Save.new ()
   val s2 = Save.new ()
in (seq [string "abc",
        save (string "d", s1),
        string "e",
        save (string "f", s2)],
    "abcdef",
    SOME 6, SOME 6)
end,
let val s1 = Save.new ()
   val s2 = Save.new ()
in (seq [string "abc",
        save (string "d", s1),
        string "e",
        save (string "fgh", s2)],
    "abcdefgh",
    SOME 8, SOME 8)
end
]

val _ =
   Assert.assert
   ("Test.Regexp.match", fn () =>
    List.forall (tests,
                 fn (r, s: string, i1, i2) =>
                 let
                    val r = compile r
                    val _ = Compiled.layoutDotToFile (r, "/tmp/z.dot")
                    fun doit m = Option.map (m (r, s, 0), Match.length)
                 in
                    i1 = doit matchShort
                    andalso i2 = doit matchLong
                 end))

val tests =
   [(string "abc", "123abc", SOME (3: int, 6: int)),
    (string "abc", "123abcde", SOME (3, 6)),
    (string "abd", "123abcde", NONE),
    (seq [string "a", star (string "ab"), string "c"],
     "1234aabababcdef", SOME (4, 12))]

val _ =
   Assert.assert
   ("Regexp.findShort", fn () =>
    List.forall
    (tests, fn (r, s, opt) =>
     opt = (Option.map
            (findShort (compile r, s, 0), fn m =>
             let val (_, {start, length}) = Substring.base (Match.all m)
             in (start, start + length)
             end))))

val _ =
   Assert.assert
   ("Regexp.findShort2", fn () =>
    List.forall
    ([(SOME (2, 4), (string "cd", "abcdef", 0)),
      (SOME (2, 4), (seq [char #"c", star (isNotChar Char.isSpace)],
                     "abcd fg", 0))],
     fn (res, (r, s, i)) =>
     res =
     Option.map (findLong (compile r, s, i), fn m =>
                 let val (_, {start, length}) = Substring.base (Match.all m)
                 in (start, start + length)
                 end)))

end
