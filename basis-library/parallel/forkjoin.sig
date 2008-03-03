signature MLTON_PARALLEL_FORKJOIN =
sig

  (* run two functions, possibly in parallel, and return their results as the
   components of a pair *)
  val fork : (unit -> 'a) * (unit -> 'b) -> 'a * 'b

  (* aggregate some intermediate results.  you can think of the arguments like
   the multiplication and unit operations of a group, along with an injection
   function from the integers.  reduce tabulates the integers from zero
   (inclusive) to "length" (exclusion), injects them into the group and then
   multiplies them up.
     
   "*" must be associative and the unit must really be the identity element of
   the group.  assuming these are true, reduce behaves according to the
   following equivalence:
     reduce _ m j u l = List.fold m u (List.tabulate (l, fn i => j i))

   maxSeq is the largest number of injections/multiplications that are run
   sequentially on a single processor.
*)
           (* maxSeq       "*"            "inj"        unit  length *)
  val reduce : int -> ('b * 'b -> 'b) -> (int -> 'b) -> 'b -> int -> 'b

end
