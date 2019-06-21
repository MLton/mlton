(* Copyright (C) 2009,2017,2019 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Cases (S: CASES_STRUCTS): CASES =
   struct
      open S

      datatype ('con, 'a) t =
         Con of ('con * 'a) vector
       | Word of WordSize.t * (WordX.t * 'a) vector

      fun equals (c1: ('con, 'a) t, c2: ('con, 'a) t,
                  eqCon: 'con * 'con -> bool,
                  eqA: 'a * 'a -> bool): bool =
         let
            fun doit (l1, l2, eq') =
               Vector.equals
               (l1, l2, fn ((x1, a1), (x2, a2)) =>
                eq' (x1, x2) andalso eqA (a1, a2))
         in
            case (c1, c2) of
               (Con l1, Con l2) => doit (l1, l2, eqCon)
             | (Word (_, l1), Word (_, l2)) => doit (l1, l2, WordX.equals)
             | _ => false
         end

      fun hd (c: ('con, 'a) t): 'a =
         let
            fun doit v =
               if Vector.length v >= 1
                  then let val (_, a) = Vector.first v
                       in a
                       end
               else Error.bug "Cases.hd"
         in
            case c of
               Con cs => doit cs
             | Word (_, cs) => doit cs
         end

      fun isEmpty (c: ('con, 'a) t): bool =
         let
            fun doit v = Vector.isEmpty v
         in
            case c of
               Con cs => doit cs
             | Word (_, cs) => doit cs
         end

      fun fold' (c: ('con, 'a) t, b, fc, fw) =
         let
            fun doit (l, f) = Vector.fold (l, b, fn ((x, a), b) => f (x, a, b))
         in
            case c of
               Con l => doit (l, fc)
             | Word (_, l) => doit (l, fw)
         end

      fun fold (c: ('con, 'a) t, b, f) =
         let
            val f = fn (_, a, b) => f (a, b)
         in
            fold' (c, b, f, f)
         end

      fun map (c: ('con, 'a) t, f): ('con, 'b) t =
         let
            fun doit l = Vector.map (l, fn (i, x) => (i, f x))
         in
            case c of
               Con l => Con (doit l)
             | Word (s, l) => Word (s, doit l)
         end

      fun forall (c: ('con, 'a) t, f: 'a -> bool): bool =
         let
            fun doit l = Vector.forall (l, fn (_, x) => f x)
         in
            case c of
               Con l => doit l
             | Word (_, l) => doit l
         end

      fun length (c: ('con, 'a) t): int = fold (c, 0, fn (_, i) => i + 1)

      fun foreach (c, f) = fold (c, (), fn (x, ()) => f x)

      fun foreach' (c, f, fc: 'con -> unit) =
         fold' (c, (), fn (c, a, ()) => (fc c; f a), fn (_, a, ()) => f a)
   end
