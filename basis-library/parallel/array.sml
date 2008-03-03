structure MLtonParallelArray =
struct

  structure B = MLtonParallelBasic
  structure F = MLtonParallelForkJoin

  fun tabulate maxSeq f size =
    let
      (* XXX check that maxSeq is large enough to ensure atomic writes *)
      val () = if maxSeq < 1 then raise B.Parallel "maxSeq must be at least 1" else ()
      val a = Array.arrayUninit size
      val () = F.reduce maxSeq
                        (fn ((), ()) => ()) 
                        (fn i => Array.update (a, i, f i)) 
                        ()
                        size
    in
      a
    end

  fun modify maxSeq f a =
    let
      (* XXX check that maxSeq is large enough to ensure atomic writes *)
      val () = if maxSeq < 1 then raise B.Parallel "maxSeq must be at least 1" else ()
      val () = F.reduce maxSeq
                        (fn ((), ()) => ()) 
                        (fn i => Array.update (a, i, 
                                               f (i, Array.sub (a, i))))
                        ()
                        (Array.length a)
    in
      ()
    end

  (* XXX check that maxSeq is large enough to ensure atomic writes *)
  fun arrayReduce maxSeq f g u a = F.reduce maxSeq 
                                            f
                                            (fn i => g (Array.sub (a, i)))
                                            u
                                            (Array.length a)
  val reduce = arrayReduce

end
