type int = Int.t
type word = Word.t
   
signature SOURCE_INFO_STRUCTS =
   sig
   end

signature SOURCE_INFO =
   sig
      include SOURCE_INFO_STRUCTS
	 
      type t

      val anonymous: Region.t -> t
      val equals: t * t -> bool
      val gc: t
      val gcArrayAllocate: t
      val hash: t -> word
      val fromC: string -> t
      val function: {name: string, region: Region.t} -> t
      val isBasis: t -> bool
      val layout: t -> Layout.t
      val main: t
      val plist: t -> PropertyList.t
      val polyEqual: t
      val toString: t -> string
      val unknown: t
   end
