(* undetermined.sml *)

(* Checks inference for non-generalised types (aka "free type variables"). *)

val f = (fn x => x) (fn x => x)
structure A = struct end
val y = f 7
;

structure A: sig val f: int -> int end =
   struct
      val f = (fn x => x) (fn x => x)
   end
;

structure A : sig val a : int list ref end =
struct
    val a = ref nil
end
;

val x = ref nil
val _ = 1 :: !x
;
;
;
val _ =
   let
      val x = ref nil
      val _ = 1 :: !x
   in
      ()
   end
;
(* 2.sml *)
val id = (fn x => x) (fn x => x)
val _ = id 13
;
structure X =
struct
    val id = (fn x => x) (fn x => x)
    val _ = id 13
end
;
(* 4.sml *)
datatype t = T
val id = (fn x => x) (fn x => x)
val _ = id T
;
(* 5.sml *)
local
   val id = (fn x => x) (fn x => x)
in
   val _ = id 13
end
;
(* 7.sml *)
val id = (fn x => x) (fn x => x)
val _ = id 13
val id = ()
;
