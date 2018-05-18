(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2004-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor MaxPow2ThatDivides (type t

                            val << : t * word -> t
                            val >> : t * word -> t
                            val <= : t * t -> bool
                            val andb: t * t -> t
                            val equals: t * t -> bool
                            val one: t
                            val orb: t * t -> t
                            val zero: t):
   sig
      val maxPow2ThatDivides: t -> word
   end =
   struct
      structure Word = Pervasive.Word

      val maxPow2ThatDivides: t -> word =
         fn i =>
         let
            (* b is the number of zero bits we are trying to peel from the
             *   bottom of i.
             * m = 2^b - 1.  m is a mask for the bits we are trying to peel.
             * 0 < a <= m.
             * ac is the number of bits that we have already peeled off.
             *)
            fun down (b: word, m: t, i: t, ac: word): word =
               let
                  val b = Word.>> (b, 0w1)
               in
                  if b = 0w0
                     then ac
                  else
                     let
                        val m = >> (m, b)
                        val a = andb (i, m)
                        val (i, ac) =
                           if equals (a, zero)
                              then (>> (i, b), ac + b)
                           else (a, ac)
                     in
                        down (b, m, i, ac)
                     end
               end
            fun up (b: word, m: t): word =
               let
                  val a = andb (i, m)
               in
                  if equals (a, zero)
                     then up (Word.<< (b, 0w1), orb (m, << (m, b)))
                  else down (b, m, a, 0w0)
               end
         in
            if i <= zero
               then Error.bug "MaxPow2ThatDivides.maxPow2ThatDivides: i <= 0"
            else up (0w1, one)
         end
   end
