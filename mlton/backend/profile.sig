type int = Int.t
type word = Word.t
   
signature PROFILE_STRUCTS = 
   sig
      structure Machine: MACHINE
      structure Rssa: RSSA
      sharing Machine.ProfileLabel = Rssa.ProfileLabel
   end

signature PROFILE = 
   sig
      include PROFILE_STRUCTS
      
      val profile:
	 Rssa.Program.t
	 -> Rssa.Program.t * ({frames: Rssa.Label.t vector}
			      -> Machine.ProfileInfo.t option)
   end
