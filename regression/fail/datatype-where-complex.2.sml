(* This should fail because 'a -> 'b is not a type name;
 * The Defn does not treat -> as a TyName(2), but rather as a distinct
 * sub-class of Type. *)
signature S =
   sig
      datatype ('a, 'b) t = T of 'a -> 'b
   end where type ('a, 'b) t = 'a -> 'b

(* Similarly, this should fail because 'a * 'b is not a type name;
 * it is a synonym for {1: 'a, 2: 'b}, which is RowType. *)
signature S =
   sig
      datatype ('a, 'b) t = T of 'a * 'b
   end where type ('a, 'b) t = 'a * 'b

(* Similarly, this should fail because 'a * 'b is not a type name;
 * it is a synonym for {1: 'a, 2: 'b}, which is RowType. *)
signature S =
   sig
      datatype t = T of unit
   end where type t = unit

(* On the other hand, The Defn does treat 'ref' and 'int' and other
 * primitive types as TyName(k); see Appendix C.  Hence, the following
 * should succeed. *)
signature S =
   sig
      datatype 'a t = T of 'a ref
   end where type 'a t = 'a ref
signature S =
   sig
      datatype t = T of int
   end where type t = int
