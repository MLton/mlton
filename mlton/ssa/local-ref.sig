signature LOCAL_REF_STRUCTS =
   sig
      include RESTORE
   end

signature LOCAL_REF =
   sig
      include LOCAL_REF_STRUCTS

      (* Local ref elimination. *)
      val eliminate: Program.t -> Program.t
   end
