abstype t = T
with
   val eq = op =
end
val _ = eq (3, 3)

abstype t = T
with
   val t = T
   val eq = op =
end
val _ = eq (t, t)

abstype t = T
with
   val t = T
   val eq = op =
   val _ = eq (t, t)
end
val _ = eq (2, 3)

abstype t = T
with
   val t = T
   val eq = op =
   val _ = eq (t, t)
end
val _ = eq (t, t)

abstype t = T
with
   val t = T
   val eq = op =
   val _ = eq (t, t) andalso eq (2, 3)
end
val _ = eq (2, 3)

(* with abstype *)

structure S =
  struct
    abstype s = S
    with
      val a = S
    end
  end

signature F = 
  sig 
    val b : S.s
  end

functor F() : F =
  struct
    type s = S.s
    val b = S.a
  end


functor K() =
  struct
    structure F = F()
  end

structure K = K()

(* abstype.sml *)

(* Checks equality inferred for abstype environments. *)

abstype t = T with
    datatype u = U of t
    val eq = op=
end

fun eq1(t1, t2) = U t1 = U t2;
fun eq2(t1, t2 : t) = eq(t1, t2);
