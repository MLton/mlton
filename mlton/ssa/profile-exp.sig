signature PROFILE_EXP_STRUCTS =
   sig
   end

signature PROFILE_EXP =
   sig
      include PROFILE_EXP_STRUCTS

      structure SourceInfo: SOURCE_INFO

      datatype t =
	 Enter of SourceInfo.t
       | Leave of SourceInfo.t

      val layout: t -> Layout.t
   end
