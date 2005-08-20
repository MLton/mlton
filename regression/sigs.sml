signature S = sig end

functor f () =
  struct
    structure K : S =
      struct
      end
  end


signature A = 
  sig type t val a : t
  end
            
signature B =
  sig
    type s
    structure A : A where type t= s
    val a : s
  end

signature C = 
  sig
    structure A : A
    structure B : B
    sharing type A.t = B.s = B.A.t
  end

structure A = 
  struct
    datatype t = A | B
    val a = A
    val b = B
  end

structure A' = A : A
structure A'' : A = A
val test1 = A'.a = A''.a