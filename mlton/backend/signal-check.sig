type int = Int.t
type word = Word.t
   
signature SIGNAL_CHECK_STRUCTS = 
   sig
      structure Rssa: RSSA
   end

signature SIGNAL_CHECK = 
   sig
      include SIGNAL_CHECK_STRUCTS

      val insert: Rssa.Program.t -> Rssa.Program.t
   end
