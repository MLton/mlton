(* Verifies that signature constraints change status *)
structure S:
   sig
      val E: exn
   end =
   struct
      exception E
   end

local open S
in val E = E
end

val e = E

structure S:
   sig
      type t
      val A: t
   end =
   struct
      datatype t = A
   end

local
   open S
in
   val A = A
end

val a = A
   
