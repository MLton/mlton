(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

type word = Word.t

structure String: STRING =
   struct
      open String1

      fun existsi (s, f) = Int.exists (0, size s, fn i => f (i, sub (s, i)))

      fun keepAll (s: t, f: char -> bool): t =
         implode (List.rev
                  (fold (s, [], fn (c, ac) => if f c then c :: ac else ac)))
         
      fun memoizeList (init: string -> 'a, l: (t * 'a) list): t -> 'a =
         let
            val set: (word * t * 'a) HashSet.t = HashSet.new {hash = #1}
            fun lookupOrInsert (s, f) =
               let
                  val hash = hash s
               in HashSet.lookupOrInsert
                  (set, hash,
                   fn (hash', s', _) => hash = hash' andalso s = s',
                   fn () => (hash, s, f ()))
               end
            val _ =
               List.foreach (l, fn (s, a) =>
                             ignore (lookupOrInsert (s, fn () => a)))
         in
            fn s => #3 (lookupOrInsert (s, fn () => init s))
         end

      fun memoize init = memoizeList (init, [])

      fun posToLineCol (s: string): int -> {line: int, col: int} =
         let
            open Int
            val lineStarts =
               Array.fromList
               (List.rev (foldi (s, [0], fn (i, c, is) =>
                                 if c = #"\n"
                                    then (i + 1) :: is
                                 else is)))
            fun find (pos: int) =
               let
                  val line =
                     valOf (BinarySearch.largest (lineStarts, fn x => x <= pos))
               (* The 1+'s are to make stuff one based *)
               in {line = 1 + line,
                   col = 1 + pos - Array.sub (lineStarts, line)}
               end
         in find
         end

      fun substituteFirst (s, {substring, replacement}) =
         case findSubstring (s, {substring = substring}) of
            NONE => s
          | SOME i =>
               let
                  val n = length substring
                  val prefix = Substring.substring (s, {start = 0, length = i})
                  val suffix = Substring.extract (s, i + n, NONE)
               in
                  Substring.concat [prefix, Substring.full replacement, suffix]
               end
      fun substituteAll (s, {substring, replacement}) =
         case findSubstring (s, {substring = substring}) of
            NONE => s
          | SOME i =>
               let
                  val ls = length s
                  val lss = length substring
                  val prefix = dropSuffix (s, ls - i)
                  val suffix = substituteAll (dropPrefix (s, i + lss),
                                              {substring = substring, 
                                               replacement = replacement})
               in
                  concat [prefix, replacement, suffix]
               end
   end

structure ZString = String (* CM bug ?? -- see instream.sml *)
