(* Required functors *)

(* Optional functors *)
functor PrimIO (S: PRIM_IO_ARG) = PrimIO (S)
functor StreamIO (S: STREAM_IO_ARG) = StreamIO (S)
functor ImperativeIO (S: IMPERATIVE_IO_ARG) = ImperativeIO (S)
