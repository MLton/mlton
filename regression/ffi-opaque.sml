structure S:>
   sig
      type t

      val x: t
      val g: t -> unit
   end =
   struct
      type t = int

      val x = 13

      fun g x = ()
   end

val f = _import "f": S.t -> unit;

val _ = fn () => f S.x

val e = _export "g1": (S.t -> unit) -> unit;

val _ = fn () => e S.g

structure S:>
   sig
      type t

      val f: t -> unit
      val x: t
   end =
   struct
      type t = int -> unit

      fun f g = g 13

      fun x _ = ()
   end

val p = _import "f": S.t;

val _ = fn () => S.f p

val e = _export "g2": S.t -> unit;

val _ = fn () => e S.x
