type int = Int.t
type word = Word.t
   
signature SSA_TO_RSSA_STRUCTS =
   sig
      structure Rssa: RSSA
      structure Ssa: SSA
      sharing Rssa.Atoms = Ssa.Atoms
      sharing Rssa.Func = Ssa.Func
      sharing Rssa.Label = Ssa.Label
      sharing Rssa.Return = Ssa.Return
      sharing Rssa.Handler = Ssa.Handler
   end

signature SSA_TO_RSSA =
   sig
      include SSA_TO_RSSA_STRUCTS
	 
      val convert: Ssa.Program.t -> Rssa.Program.t
   end
