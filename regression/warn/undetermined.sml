(* 1.sml *)
val id = (fn x => x) (fn x => x)
;
structure B : sig end =
struct
    val a = ref nil
end
;
(* 3.sml *)
val id = (fn x => x) (fn x => x)
;
val _ = id 13
;
(* 6.sml *)
val id = (fn x => x) (fn x => x)
val id = ()
;
val x = ref [];
val _ = 1 :: !x
;
val x = ref nil
signature S = sig end
val _ = 1 :: !x
;
val x = ref nil;
val _ = () :: !x


