(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature PARSE_SEXP_STRUCTS = 
   sig
      structure Sexp: SEXP
   end

signature PARSE_SEXP = 
   sig
      include PARSE_SEXP_STRUCTS

      type 'a t

      exception Parse
      val parse: 'a t * Sexp.t -> 'a

      val anything: Sexp.t t
      val atom: (string -> 'a) -> 'a t
      val string: string -> unit t
      val anyString: string t
      val cons: 'a t * 'b t -> ('a * 'b) t
      val list: 'a t -> 'a list t
      val tuple2: 'a t * 'b t -> ('a * 'b) t
      val tuple3: 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
      val tuple4: 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) t
      val tuple5: 'a t * 'b t * 'c t * 'd t * 'e t -> ('a * 'b * 'c * 'd * 'e) t
      val wrap: 'a t * ('a -> 'b) -> 'b t
      val or: 'a t list -> 'a t
      val fold: 'a t * 'b * ('a * 'b -> 'b) -> 'b t
   end
