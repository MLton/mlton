signature S =
   sig
      type t
      structure S:
         sig
            type 'a t
            val x: 'a t
         end
   end

structure S:
   sig
      datatype ('a, 'b) t = T of 'a
   end =
   struct
      datatype ('b, 'a) t = T of 'b
   end

functor F (eqtype t
           datatype u = U of t
           eqtype v
           sharing type t = v) =
   struct
      fun f (u: u) = u = u
   end

functor F (type t
           eqtype u
           sharing type t = u) =
   struct
      fun f (x: t) = x = x
   end

signature S1 =
   sig
      datatype t = T
   end

signature S2 =
   sig
      eqtype u
      structure S: S1 where type t = u
   end;

signature S1 =
   sig
      datatype t = T
   end

signature S2 =
   sig
      datatype u = U 
      structure S: S1 where type t = u
   end;

signature S =
   sig
      datatype t = T
   end where type t = int

signature S =
   sig
      type t
      type u
      type v = t
      sharing type u = v
   end

signature S =
   sig
      type 'a t
      type 'a u
      type 'a v = 'a t
      sharing type u = v
   end

signature S =
   sig
      type t
      datatype u = U
      sharing type u = t
   end

signature S =
   sig
      type t
      structure Z:
         sig
            datatype u = U
         end
      sharing type Z.u = t
   end

signature S =
   sig
      eqtype t
      structure Z:
         sig
            datatype u = U
         end where type u = t
   end

structure S:
   sig
      eqtype t
      structure Z:
         sig
            datatype u = U
         end where type u = t
   end =
   struct
      structure Z =
         struct
            datatype u = U
         end
      type t = Z.u
   end

functor F () = struct end

functor F (type t) = struct type u = t end

functor F (val x: int) = struct val y = x end

functor F (structure S: sig end) = struct open S end

functor F (type t
           type u
           sharing type t = u) =
   struct
      val id: t -> u = fn x => x
   end

functor F (eqtype t) = struct fun f (x: t) = x = x end

functor F (structure S:
              sig
                 type t
              end
           structure T:
              sig
                 type t
              end
           sharing S = T) =
   struct
      val id: S.t -> T.t = fn x => x
   end

functor F (datatype 'a t = T of 'a * 'a) =
   struct
      val _ = T (13, 14)
   end

functor F (type ('a, 'b) t
           type 'a u = ('a, int) t
           val f: (bool, 'b) t -> real
           val u: bool u) =
   struct
      val _ = f u
   end
              
functor F (datatype t = T
           datatype u = U of t) =
   struct
      fun f (x: u) = x = x
   end

structure S:
   sig
      val x: unit list
   end =
   struct
      val x = []
   end

structure S:
   sig
      val x: 'a list
   end =
   struct
      val x = []
   end

structure S:
   sig
      val x: ''a list
   end =
   struct
      val x = []
   end

structure S:
   sig
      val f: ''a -> ''a list
   end =
   struct
      fun f x = [x]
   end

structure S:
   sig
      type 'a t

      val T: int t
   end =
   struct
      datatype 'a t = T
   end

structure S:
   sig
      val f: 'a list -> 'a list
   end =
   struct
      fun f _ = []
   end
val z = S.f [1, 2, 3]

structure S = struct datatype t = T end
functor F () = S
structure S1 = F ()
structure S2 = F ()
val _ = S1.T = S2.T

datatype t = T
functor F () =
   struct
      datatype t = datatype t
   end
structure S1 = F ()
structure S2 = F ()
val _ = S1.T = S2.T

signature SIG = sig type t end
functor F (S: sig type t end): SIG = S
structure S:>
   sig
      structure T: sig type t end
      structure U: SIG
      sharing type T.t = U.t
   end =
   struct
      structure T = struct type t = unit end
      structure U = F (type t = unit)
   end
val f = fn z: S.T.t => z: S.U.t

structure S:>
   sig
      eqtype t
      val x: t
   end =
   struct
      type t = unit
      val x = ()
   end
val _ = S.x = S.x

val _ = fn x: LargeInt.int => x: IntInf.int

signature S =
   sig
      type t
   end
structure S:> S =
   struct
      datatype t = T
   end
signature T =
   sig
      type u
      include S where type t = S.t
   end
structure T:> T =
   struct
      datatype u = U
      type t = S.t
   end
val _: T.t -> S.t = fn x => x

signature SIG =
   sig
      type u
      type v = u
   end where type v = int
structure S: SIG =
   struct
      type u = int
      type v = int
   end

functor F () =
   struct
      val A = 0
   end
structure S =
   struct
      datatype z = A
      structure F = F ()
   end
