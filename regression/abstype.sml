
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

(*
(* with opaque constraints instead of abstype *)

structure S' :> sig type s val a : s end =
  struct
    datatype s = S
    val a = S
  end

signature F' = 
  sig 
    val b : S'.s
  end

functor F'() : F' =
  struct
    type s = S'.s
    val b = S'.a
  end

structure F' = F'()

*)