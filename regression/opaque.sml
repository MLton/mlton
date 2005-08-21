(* From Chapter 5 of Elsman's PhD thesis; these examples should
 * elaborate. *)

(* Decrease in generativity *)

functor f() = struct type a = int 
                     val a = 232
                     val pr = Int.toString 
              end :> sig type a
                         val a : a                         
                         val pr : a -> string
                     end

structure f = f()
val _ = print ("f.a = " ^ f.pr f.a ^ "\n")

functor g() = struct datatype a = A | B
                     fun pr_a A = "A"
                       | pr_a B = "B"
                     val pr_b = pr_a
                     type b = a
              end :> sig type a type b 
                         val A : a
                         val B : b
                         val pr_a : a -> string
                         val pr_b : b -> string
                     end

structure g = g()
val _ = print ("g.A = " ^ g.pr_a g.A ^ "\n")
val _ = print ("g.B = " ^ g.pr_b g.B ^ "\n")


functor h(s : sig type a val pr : a -> string val a : a end) = 
  struct 
    val pr = s.pr 
    val b = s.a
    type b = s.a 
  end :> sig type b 
             val pr : b -> string 
             val b : b 
         end

structure h = h(struct type a = int val pr = Int.toString val a = 343 end)
val _ = print ("h.b = " ^ h.pr h.b ^ "\n")

(* Increase in generativity *)

functor i() = struct datatype a = A
                     and b = B | C
                     type c = a * b
                     val c = (A,C)
                     fun pr (A,B) = "(A,B)"
                       | pr (A,C) = "(A,C)"
              end :> sig type c val c : c val pr : c -> string end

structure i = i()
val _ = print ("i.c = " ^ i.pr i.c ^ "\n")

(* Signature S below is well-formed, but after opacity elimination it
 * is not. No real structure (i.e., a structure existing outside of a
 * functor body) can match the signature S. The signature should
 * elaborate, however. *)

structure S = struct type s = int * int
              end :> sig eqtype s end

signature S = sig datatype u = A 
              end where type u = S.s
