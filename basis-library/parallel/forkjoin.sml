structure MLtonParallelForkJoin :> MLTON_PARALLEL_FORKJOIN = 
struct

  structure B = MLtonParallelBasic

  val fetchAndAdd = _import "Parallel_fetchAndAdd": Int32.int ref * Int32.int -> Int32.int;

  datatype 'a result = 
      NotYet
    | Finished of 'a
    | Raised of exn

  fun fork (f, g) =
      let
        val c = ref 0
        val l = ref NotYet
        val r = ref NotYet
                
        fun wrap k h res () = 
            let
              val v = Finished (h ())
                      handle e => Raised e
              val () = res := v
              val t = fetchAndAdd (c, 1)
            in
              if t = 1 then 
                B.resume (k, (!l, !r))
              else 
                B.return ()
            end
      in
        case B.suspend (fn k => [wrap k f l, wrap k g r])
         of (Finished a, Finished b) => (a, b)
          | (Raised e, _) => raise e
          | (_, Raised e) => raise e
          | _ => raise B.Parallel "impossible"
      end

  fun reduce maxSeq f g u n =
    let
      val () = if maxSeq < 1 then raise B.Parallel "maxSeq must be at least 1" else ()

      fun wrap i l () =
        if l <= maxSeq then
           let
             val stop = i + l
             fun loop j v = if j = stop then v
                            else loop (j + 1) (f (v, g j))
           in
             loop i u
           end
        else
          let
            val l' = l div 2
          in
            f (fork (wrap i l',
                     wrap (i + l') (l - l')))
          end
    in
      wrap 0 n ()
    end

end
