type int = Int.t
type word = Word.t

signature PROFILE_LABEL_STRUCTS =
   sig
   end

signature PROFILE_LABEL =
   sig
      type t
	
      val clear: t -> unit
      val equals: t * t -> bool
      val layout: t -> Layout.t
      val new: unit -> t
      val plist: t -> PropertyList.t
      val toString: t -> string
   end
