signature KNOWN_CASE_STRUCTS = 
  sig
    include RESTORE
  end

signature KNOWN_CASE =
  sig
    include KNOWN_CASE_STRUCTS

    val simplify: Program.t -> Program.t
  end