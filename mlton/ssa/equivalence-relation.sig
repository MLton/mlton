(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
type int = Int.t

signature EQUIVALENCE_RELATION_STRUCTS = 
   sig
   end

signature EQUIVALENCE_RELATION = 
   sig
      include EQUIVALENCE_RELATION_STRUCTS
      
      (* An equivalence relation on integers. *)
      type t

      (* universal n = r, where r(x, y) iff 0 <= x < n andalso 0 <= y < n *)
      (* make(l, f) is the equivalence relation r such that
       * r(x, y) iff f(List.nth(l, x), List.nth(l, y))
       * f must be an equivalence relation on 'a (this is not checked).
       *)
      (* classes r returns the equivalence classes in the relation *)
      (* refine(r, f) is the equivalence relation r' such that
       * r'(x, y) iff r(x, y) andalso f(x, y).
       * f must be an equivalence relation (this is not checked).
       *)
      val areEquivalent : t * int * int -> bool
      val classes : t -> int list list
      val equals : t * t -> bool
      val layout : t -> Layout.t
      val make : 'a list * ('a * 'a -> bool) -> t
      val refine : t * (int * int -> bool) -> t
      val universal : int -> t
   end
