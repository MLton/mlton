signature SOURCE_INFO_STRUCTS =
   sig
   end

signature SOURCE_INFO =
   sig
      include SOURCE_INFO_STRUCTS
	 
      type t

      val bogus: t
      val fromRegion: Region.t -> t
      val layout: t -> Layout.t
      val main: t
      val polyEqual: t
      val toString: t -> string
   end
