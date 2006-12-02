signature NET_SERV_DB =
   sig
      type entry
      val name: entry -> string
      val aliases: entry -> string list
      val port: entry -> int
      val protocol: entry -> string
      val getByName: string * string option -> entry option
      val getByPort: int * string option -> entry option
   end
