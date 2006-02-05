signature COMMAND_LINE =
   sig
      val name: unit -> string 
      val arguments: unit -> string list
   end
