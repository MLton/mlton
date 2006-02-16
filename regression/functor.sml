signature A = sig type t val a : t end

functor F(A : sig type t val pr : t -> string end) = 
  struct datatype k = A of A.t 
         fun pr (A t) = "A(" ^ A.pr t ^ ")"
  end

functor G(A : A) =
  struct type t = A.t val b = A.a end

functor H(A : sig type t type s val a : s
                  sharing type t = s 
                  val pr : t -> string 
              end) =
  struct
    structure A1 : sig type t val pr : t -> string end = A
    structure A2 = F(A1)
    structure A3 = G(A)
    val a = A2.A A.a
    val _ = print ("value is " ^ A2.pr a ^ "\n")
  end

structure B1 = 
  struct 
    datatype t = J | K 
    type s = t
    val a = K
    fun pr K = "K"
      | pr J = "J"
  end

structure B2 =
  struct open B1
    val a = J
  end

structure H1 = H(B1)
structure H2 = H(B2)

signature S = sig end

functor F (): S = struct end

signature S = sig val y: int end

structure C = F ()
