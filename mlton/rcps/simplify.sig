
signature SIMPLIFY_STRUCTS =
  sig
    include TYPE_CHECK
  end

signature SIMPLIFY =
  sig
    include SIMPLIFY_STRUCTS    

    val simplify: Program.t -> Program.t
  end