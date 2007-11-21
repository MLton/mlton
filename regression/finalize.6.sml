structure Bug =
struct 

  structure F = MLton.Finalizable

  fun new_t () =
      let 
        val p = 0
        val t = F.new p
        fun finalize x = ()
      in
        F.addFinalizer(t,finalize);
        t
      end

  fun from_string (_:string) = 
      let
        val x = new_t ()
      in
        F.withValue(x,fn p => ());
        x
      end

  val zero = from_string "0.0"

  (* NOTE: I removed the F.withValue lines in an attempt to make the
   code simpler, but the bug didn't manifest itself.  So I think these
   lines are critical. *)
  fun plus (x,y) = 
      let
        val z = new_t ()
      in
        F.withValue(x,fn xp => 
          F.withValue(y,fn yp => 
            F.withValue(z,fn zp => 
              let in
                z
              end)))
      end

end
 
structure B = Bug

fun bigsum (n,store) =
    if n = 0 then store else
    let
      val _ = if Int.mod(n,10000) = 0 then print (Int.toString n ^ "\n") else ()
    in
      bigsum(Int.-(n,1),B.plus(store,B.from_string(Int.toString n ^ ".0")))
    end

val bigsum = (fn n => bigsum(n,B.zero))

val x = bigsum 5000000
