type int = Int.t
type word = Word.t
   
signature REAL_X_STRUCTS = 
   sig
      structure RealSize: REAL_SIZE
   end

signature REAL_X = 
   sig
      include REAL_X_STRUCTS

      (* reals of all RealSize.t sizes. *)
      type t

      val equals: t * t -> bool
      val hash: t -> word
      val layout: t -> Layout.t
      val make: string * RealSize.t -> t
      val size: t -> RealSize.t
      val toString: t -> string
   end
