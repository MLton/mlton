
signature TYPE_CHECK_STRUCTS =
  sig
    include RCPS_TREE
  end

signature TYPE_CHECK =
  sig
    include TYPE_CHECK_STRUCTS

    val typeCheck: Program.t -> unit
  end