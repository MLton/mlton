type int = Int.t
type word = Word.t
   
signature SOURCE_INFO_STRUCTS =
   sig
   end

signature SOURCE_INFO =
   sig
      include SOURCE_INFO_STRUCTS
	 
      type t

      val equals: t * t -> bool
      val fromRegion: Region.t -> t
      val hash: t -> word
      val isBasis: t -> bool
      val layout: t -> Layout.t
      val main: t
      val polyEqual: t
      val toString: t -> string
      val unknown: t
   end
