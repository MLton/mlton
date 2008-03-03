structure MLtonParallelInternal =
struct

  val numberOfProcessors = Int32.toInt ((_import "Parallel_numberOfProcessors": unit -> Int32.int;) ())

  val processorNumber = _import "Parallel_processorNumber": unit -> Int32.int;

end
