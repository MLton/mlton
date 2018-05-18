(* sharing.sml *)

(* Checks treatment of sharing constraints. *)

signature S =
sig
    type t
    type s = t
    sharing type t = s
end;

signature T =   (* from SML/NJ doc *)
sig
    type s
    structure A :
    sig
        datatype d = D of s
        datatype t = K
    end
    sharing type s = A.t
end;

(* Check that multiple sharing equations is all pairs. *)
signature S =
   sig
      structure T: sig end
      structure U: sig type t = int end
      sharing T = U
   end

functor F (structure A: sig type t end
           structure B: sig end
           structure C: sig type t end
           sharing A = B = C) =
   struct
      val _: A.t -> C.t = fn x => x
   end

functor F (structure A: sig type t end
           structure B: sig type u end
           structure C: sig type t end
           structure D: sig type u end
           sharing A = B = C = D) =
   struct
      val _: A.t -> C.t = fn x => x
      val _: B.u -> D.u = fn x => x
   end

(* Check that sharing doesn't mistakenly share structures that only differ in
 * free flexible tycons.
 *)

signature T =
   sig
      type t
      structure U:
         sig
            val x: t
         end
   end

structure S:
   sig
      structure T1: T
      structure T2: T
      sharing T1.U = T2.U
   end =
   struct
      structure T1 =
         struct
            type t = int
            structure U =
               struct
                  val x = 13
               end
         end
      structure T2 =
         struct
            type t = real
            structure U =
               struct
                  val x = 13.0
               end
         end
   end
;
