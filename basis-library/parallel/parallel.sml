structure MLtonParallel :> MLTON_PARALLEL =
struct

  structure Basic : MLTON_PARALLEL_BASIC = MLtonParallelBasic
  structure ForkJoin : MLTON_PARALLEL_FORKJOIN = MLtonParallelForkJoin
  structure Array : MLTON_PARALLEL_ARRAY = MLtonParallelArray

end
