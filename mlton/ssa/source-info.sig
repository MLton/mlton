signature SOURCE_INFO =
   sig
      type t

      val bogus: t
      val fromRegion: Region.t -> t
      val layout: t -> Layout.t
      val main: t
      val polyEqual: t
      val toString: t -> string
   end
