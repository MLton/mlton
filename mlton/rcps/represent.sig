
signature REPRESENT_STRUCTS =
  sig
    structure Cps: CPS
    structure Rcps: RCPS
    sharing Cps.Atoms = Rcps.Atoms
    sharing type Cps.Func.t = Rcps.Func.t
    sharing type Cps.Jump.t = Rcps.Jump.t
  end

signature REPRESENT =
  sig
    include REPRESENT_STRUCTS

    val represent: Cps.Program.t -> Rcps.Program.t
  end