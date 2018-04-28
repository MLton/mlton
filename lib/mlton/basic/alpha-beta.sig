(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature ALPHA_BETA_STRUCTS = 
   sig
      structure Value:
         sig
            include ORDER

            val largest: t
            val smallest: t
            val move: t -> t
            val unmove: t -> t
         end

      structure State:
         sig
            type t

            val succ: t -> t list
            datatype value =
               Leaf of Value.t
             | NonLeaf of {lower: Value.t, upper: Value.t}
            val evaluate: t -> value
            val layout: t -> Layout.t
         end

      structure Cache:
         sig
            type 'a t

            val peek: 'a t * State.t -> {value: 'a option,
                                         update: 'a -> unit}
         end
   end

signature ALPHA_BETA = 
   sig
      include ALPHA_BETA_STRUCTS

      (* return v s.t. a <= v <= b
       * andalso |v - v'| is minimal, where v' is maximum value.
       *)
      val alphaBeta: State.t * Value.t * Value.t -> Value.t

      structure Interval:
         sig
            type t

            val make: {lower: Value.t, upper: Value.t} -> t
            val all: t
            val point: Value.t -> t
            val isPoint: t -> bool
            val lower: t -> Value.t
            val upper: t -> Value.t
            val layout: t -> Layout.t
         end

      (* Return closest value in interval to maximum value. *)
      (* May modify the cache. *)
      val alphaBetaCache: State.t * Interval.t * Interval.t Cache.t -> Value.t
   end
