(* undetermined.sml *)

(* Checks inference for non-generalised types (aka "free type variables"). *)

val a = ref nil
val _ = a := [1];

structure A : sig val a : int list ref end =
struct
    val a = ref nil
end;

structure B : sig end =
struct
    val a = ref nil
end;
