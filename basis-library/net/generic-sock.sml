structure GenericSock : GENERIC_SOCK =
   struct
      structure Prim = Primitive.Socket.GenericSock
      structure PE = Posix.Error

      fun intToSock i = Socket.wordToSock (SysWord.fromInt i)

      fun socket' (af, st, p) =
	intToSock
	(PE.checkReturnResult
	 (Prim.socket (NetHostDB.addrFamilyToInt af, st, p)))

      fun socketPair' (af, st, p) =
	let
	  val s1 = ref 0
	  val s2 = ref 0
	  val _ =
	     PE.checkResult
	     (Prim.socketPair (NetHostDB.addrFamilyToInt af, st, p, s1, s2))
	in
	  (intToSock (!s1), intToSock (!s2))
	end

      fun socket (af, st) = socket' (af, st, 0)

      fun socketPair (af, st) = socketPair' (af, st, 0)
   end
