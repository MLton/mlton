(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature EUCLIDEAN_RING_STRUCTS =
   sig
      include RING_WITH_IDENTITY

      (* Two elements a, b are "unit equivalent" if there is a unit element u
       * such that au = b.
       *)

      val divMod: t * t -> t * t
      val metric: t -> Pervasive.IntInf.int
      (* Monics should be an (infinite) stream of all (except for zero and one)
       * the representatives of the unit equivalence classes in nondecreasing
       * order of metric.
       *)
      val monics: t Stream.t
      (* Map an element to the representative of its unit equivalence class. *)
      val unitEquivalent: t -> t
   end

signature EUCLIDEAN_RING =
   sig
      include EUCLIDEAN_RING_STRUCTS

      val div: t * t -> t
      val divides: t * t -> bool
      val extendedEuclid: t * t -> t * t * t
      val extendedEuclidTerm: t * t * (t * t -> bool) -> t * t * t
      val factor: t -> (t * Pervasive.Int.int) list
      val gcd: t * t -> t
      val isComposite: t -> bool
      val isPrime: t -> bool
      val lcm: t * t -> t
      val primes: t Stream.t
      val mod: t * t -> t
   end
