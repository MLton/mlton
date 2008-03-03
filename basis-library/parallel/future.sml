structure MLtonParallelFuture :> MLTON_PARALLEL_FUTURE =
struct

  structure B = MLtonParallelBasic

  val fetchAndAdd = _import "Parallel_fetchAndAdd": Int32.int ref * Int32.int -> Int32.int;

  datatype 'a result =
      NotYet 
    | Finished of 'a
    | Raised of exn

  type 'a t = 'a result ref * 'a result B.t option ref * int ref

  fun future f =
    let 
      val r = ref NotYet
      val n = ref NONE
      val c = ref 0
      val () = B.addWork 
        [fn () => 
         let
           val res = Finished (f ())
                       handle e => Raised e
           val () = r := res
           val t = fetchAndAdd (c, 1)
         in
           if t = 0 then B.return ()
           else B.resume (valOf (!n), res)
         end]
    in
      (r, n, c)
    end

  fun force (r, n, c) =
    let 
      val res = if !c = 1 then
                  B.continue (fn () => !r)
                else
(*
                  B.suspend (fn k =>
                                let 
                                  val () = n := SOME k
                                  val t = fetchAndAdd (c, 1)
                                in
                                  if t = 0 then []
                                  else [fn () => B.resume (k, !r)]
                                end)
*)
                  B.suspend (fn k =>
                                let 
                                  val () = n := SOME k
                                in
                                  [fn () => 
                                      let 
                                        val t = fetchAndAdd (c, 1)
                                      in
                                        if t = 0 then B.return () else B.resume (k, !r)
                                      end]
                                end)

    in
      case res of 
        Finished v => v
      | Raised e => raise e
      | NotYet => raise B.Parallel "got NotYet in force!"
    end

end
