signature MLTON_PARALLEL =
sig

  structure Basic : MLTON_PARALLEL_BASIC
  structure ForkJoin : MLTON_PARALLEL_FORKJOIN
  structure Array : MLTON_PARALLEL_ARRAY

end
