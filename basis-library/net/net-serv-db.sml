structure NetServDB: NET_SERV_DB =
   struct
      structure Prim = Primitive.NetServDB

      datatype entry = T of {name: string,
			     aliases: string list,
			     port: int,
			     protocol: string}

      local
	fun make s (T r) = s r
      in
	val name = make #name
	val aliases = make #aliases
	val port = make #port
	val protocol = make #protocol
      end

      local
	fun get (b: bool): entry option =
	  if b
	    then let
		   val name = C.CS.toString (Prim.entryName ())
		   val numAliases = Prim.entryNumAliases ()
		   fun fill (n, aliases) =
		     if n < numAliases
		       then let
			      val alias =
				C.CS.toString (Prim.entryAliasesN n)
			    in
			      fill (n + 1, alias::aliases)
			    end
		       else List.rev aliases
		   val aliases = fill (0, [])
		   val port = Net.ntohs (Prim.entryPort ())
		   val protocol = C.CS.toString (Prim.entryProtocol ())
		 in
		   SOME (T {name = name,
			    aliases = aliases,
			    port = port,
			    protocol = protocol})
		 end
	    else NONE
      in
	fun getByName (name, proto) = 
	  case proto of
	    SOME proto => get (Prim.getByName (NullString.nullTerm name,
					       NullString.nullTerm proto))
	  | NONE => get (Prim.getByNameNull (NullString.nullTerm name))
	fun getByPort (port, proto) = 
	  let
	    val port = Net.htons port
	  in
	    case proto of
	       NONE => get (Prim.getByPortNull port)
	     | SOME proto =>
		  get (Prim.getByPort (port, NullString.nullTerm proto))
	  end
      end
   end
