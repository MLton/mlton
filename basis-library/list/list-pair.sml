(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure ListPair: LIST_PAIR =
   struct
      exception UnequalLengths

      fun id x = x

      fun ul _ = raise UnequalLengths

      fun unzip l =
         List.foldr (fn ((x, y), (xs, ys)) => (x :: xs, y :: ys)) ([], []) l

      fun foldl' w f b (l1, l2) =
         let
            fun loop (l1, l2, b) =
               case (l1, l2) of
                  ([], []) => b
                | (x1 :: l1, x2 :: l2) => loop (l1, l2, f (x1, x2, b))
                | _ => w b
         in
            loop (l1, l2, b)
         end

      fun foldl f = foldl' id f

      fun foldlEq f = foldl' ul f

      fun foldr' w f b (l1, l2) =
         let
            fun loop (l1, l2) =
               case (l1, l2) of
                  ([], []) => b
                | (x1 :: l1, x2 :: l2) => f (x1, x2, loop (l1, l2))
                | _ => w b
         in
            loop (l1, l2)
         end

      fun foldr f = foldr' id f

      fun foldrEq f = foldr' ul f

      fun zip' w (l1, l2) =
         rev (foldl' w (fn (x, x', l) => (x, x') :: l) [] (l1, l2))

      fun zip (l1, l2) = zip' id (l1, l2)

      fun zipEq (l1, l2) = zip' ul (l1, l2)

      fun map' w f = rev o (foldl' w (fn (x1, x2, l) => f (x1, x2) :: l) [])

      fun map f = map' id f

      fun mapEq f = map' ul f

      fun app' w f = foldl' w (fn (x1, x2, ()) => f (x1, x2)) ()

      fun app f = app' id f

      fun appEq f = app' ul f

      fun exists p (l1, l2) =
         let
            fun loop (l1, l2) =
               case (l1, l2) of
                  (x1 :: l1, x2 :: l2) => p (x1, x2) orelse loop (l1, l2)
                | _ => false
         in
            loop (l1, l2)
         end

      fun all p ls = not (exists (not o p) ls)

      fun allEq p =
         let
            fun loop (l1, l2) =
               case (l1, l2) of
                  ([], []) => true
                | (x1 :: l1, x2 :: l2) => p (x1, x2) andalso loop (l1, l2)
                | _ => false
         in
            loop
         end
   end
