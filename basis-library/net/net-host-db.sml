structure NetHostDB: NET_HOST_DB =
   struct
      structure Prim = Primitive.NetHostDB
     
      type in_addr = Word8Vector.vector
      type addr_family = int (* AF_INET *)
      datatype entry = T of {name: string,
			     aliases: string list,
			     addrType: addr_family,
			     addrs: in_addr list}

      local
	fun make s (T r) = s r
      in
	val name = make #name
	val aliases = make #aliases
	val addrType = make #addrType
	val addrs = make #addrs
      end
      fun addr entry = hd (addrs entry)

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
		   val addrType = Prim.entryAddrType ()
		   val length = Prim.entryLength ()
		   val numAddrs = Prim.entryNumAddrs ()
		   fun fill (n, addrs) =
		     if n < numAddrs
		       then let
			      val addr = Word8Array.array(length, 0wx0)
			      val _ = Prim.entryAddrsN(n, addr)
			      val addr = Word8Array.vector addr
			    in
			      fill (n + 1, addr::addrs)
			    end
		       else List.rev addrs
		   val addrs = fill (0, [])
		 in
		   SOME (T {name = name,
			    aliases = aliases,
			    addrType = addrType,
			    addrs = addrs})
		 end
	    else NONE
      in
	fun getByAddr in_addr = 
	  get (Prim.getByAddress (in_addr, Word8Vector.length in_addr))
	fun getByName name = 
	  get (Prim.getByName (String.nullTerm name))
      end

      fun getHostName () = 
	let
	  val n = 128
	  val buf = CharArray.array (n, #"\000")
	  val _ = Posix.Error.checkResult (Prim.getHostName (buf, n))
	in
	  case CharArray.findi (fn (_, c) => c = #"\000") buf of
	    NONE => CharArray.vector buf
	  | SOME (i, _) => CharArraySlice.vector (CharArraySlice.slice (buf, 0, SOME i))
	end

      fun scan reader state =
	let
	  fun scanW state =
	    case reader state of
	      SOME (#"0", state') => 
		(case reader state' of
		   SOME (#"X", state'') => 
		     StringCvt.wdigits StringCvt.HEX reader state''
		 | SOME (#"x", state'') => 
		     StringCvt.wdigits StringCvt.HEX reader state''
		 | SOME (c, state'') =>
		     if Char.isDigit c
		       then StringCvt.wdigits StringCvt.OCT reader state'
		       else SOME (0wx0, state'))
	    | _ => StringCvt.wdigits StringCvt.DEC reader state
	  fun loop (n, state, acc) =
	    if n <= 0
	      then List.rev acc
	      else let
		     fun finish (w, state) =
		       case reader state of
			 SOME (#".", state') => 
			   loop (n - 1, state', (w, state)::acc)
		       | _ => List.rev ((w, state)::acc)
		   in
		     case scanW state of
		       SOME (w, state') => finish (w, state')
		     | NONE => List.rev acc
		   end
	  val l = loop (4, state, [])
	  fun get1 w = 
	    (Word8.fromLargeWord (Word32.andb (w, 0wxFF)),
	     Word32.>>(w, 0w8))
	  fun get2 w =
	    let 
	      val (a,w) = get1 w
	      val (b,w) = get1 w
	    in (a,b,w) 
	    end
	  fun get3 w =
	    let
	      val (a,b,w) = get2 w
	      val (c,w) = get1 w
	    in (a,b,c,w)
	    end
	  fun get4 w =
	    let
	      val (a,b,c,w) = get3 w
	      val (d,w) = get1 w
	    in (a,b,c,d,w)
	    end
	  fun try l =
	    case l of 
	      [] => NONE
	    | [(w, statew)] => 
		let
		  val (d,c,b,a,w) = get4 w
		in
		  if w = 0wx0
		    then SOME (Word8Vector.fromList [a,b,c,d], statew)
		    else NONE
		end
	    | [(x, statex), (w, statew)] => 
		let
		  val (d,c,b,w) = get3 w
		  val (a,x) = get1 x
		in
		  if w = 0wx0 andalso x = 0wx0
		    then SOME (Word8Vector.fromList [a,b,c,d], statew)
		    else try [(x, statex)]
		end
	    | [(y, statey), (x, statex), (w, statew)] => 
		let
		  val (d,c,w) = get2 w
		  val (b,x) = get1 x
		  val (a,y) = get1 y
		in
		  if w = 0wx0 andalso x = 0wx0 andalso y = 0wx0
		    then SOME (Word8Vector.fromList [a,b,c,d], statew)
		    else try [(y, statey), (x, statex)]
		end
	    | [(z, statez), (y, statey), (x, statex), (w, statew)] => 
		let
		  val (d,w) = get1 w
		  val (c,x) = get1 x
		  val (b,y) = get1 y
		  val (a,z) = get1 z
		in
		  if w = 0wx0 andalso x = 0wx0 andalso y = 0wx0 andalso z = 0wx0
		    then SOME (Word8Vector.fromList [a,b,c,d], statew)
		    else try [(z, statez), (y, statey), (x, statex)]
		end
	    | _ => NONE
	in
	  try l
	end

(*
      val scan = fn _ => raise (Fail "NetHostDB.scan unimplemented")
*)
      fun fromString s = StringCvt.scanString scan s
      fun toString in_addr =
	String.concatWith "." 
	(Word8Vector.foldr (fn (w,ss) => (Word8.fmt StringCvt.DEC w)::ss) [] in_addr)
   end