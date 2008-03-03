signature MLTON_PARALLEL_ARRAY =
sig
  (* parallel versions of ordinary array operations.  maxSeq indicates the
   largest number of elements that will be computed sequentially on a single
   processor. *)
             (* maxSeq      f           n *)
  val tabulate : int -> (int -> 'a) -> int -> 'a Array.array
           (* maxSeq      f                      a *)
  val modify : int -> (int * 'a -> 'a) -> 'a Array.array -> unit

  (* see the comment for MLTON_PARALLEL_FORKJOIN.reduce *)
            (* maxSeq       "*"            "inj"      "unit"      a *)
  val reduce : int -> ('b * 'b -> 'b) -> ('a -> 'b) -> 'b -> 'a Array.array -> 'b
end
