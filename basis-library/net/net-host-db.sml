(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure NetHostDB: NET_HOST_DB_EXTRA =
   struct
      structure Prim = PrimitiveFFI.NetHostDB

      (* network byte order (big-endian) *)
      type pre_in_addr = Word8.word array
      type in_addr = Word8.word vector

      val preInAddrToWord8Array = fn a => a
      val inAddrToWord8Vector = fn v => v

      val inAddrLen = C_Size.toInt Prim.inAddrSize
      fun new_in_addr () =
        let
          val ia: pre_in_addr = Array.array (inAddrLen, 0wx0: Word8.word)
          fun finish () = Array.vector ia
        in
          (ia, finish)
        end
      fun any () =
         let
            val (wa, finish) = new_in_addr ()
            fun loop (i, acc) =
               if i >= inAddrLen
                  then ()
                  else let
                          val w = Word8.castFromSysWord (C_Int.castToSysWord acc)
                          val () =
                             Array.update
                             (wa, (inAddrLen - 1) - i, w)
                       in
                          loop (i + 1, C_Int.>> (acc, 0w4))
                       end
         in
            loop (0, Prim.INADDR_ANY)
            ; finish ()
         end

      type addr_family = C_Int.t

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
        fun get (i: C_Int.t): entry option =
          if i <> C_Int.zero
            then let
                   val name = CUtil.C_String.toString (Prim.getEntryName ())
                   val numAliases = Prim.getEntryAliasesNum ()
                   fun fill (n, aliases) =
                     if C_Int.< (n, numAliases)
                       then let
                              val alias =
                                CUtil.C_String.toString (Prim.getEntryAliasesN n)
                            in
                              fill (C_Int.+ (n, 1), alias::aliases)
                            end
                       else List.rev aliases
                   val aliases = fill (0, [])
                   val addrType = Prim.getEntryAddrType ()
                   val length = Prim.getEntryLength ()
                   val numAddrs = Prim.getEntryAddrsNum ()
                   fun fill (n, addrs) =
                     if C_Int.< (n, numAddrs)
                       then let
                              val addr = Word8Array.array (C_Int.toInt length, 0wx0)
                              val _ = Prim.getEntryAddrsN (n, Word8Array.toPoly addr)
                              val addr = Word8Vector.toPoly (Word8Array.vector addr)
                            in
                              fill (C_Int.+ (n, 1), addr::addrs)
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
           get (Prim.getByAddress (in_addr, C_Socklen.fromInt (Vector.length in_addr)))
        fun getByName name = 
           get (Prim.getByName (NullString.nullTerm name))
      end

      fun getHostName () = 
        let
          val n = 128
          val buf = CharArray.array (n, #"\000")
          val () =
             Posix.Error.SysCall.simple
             (fn () => Prim.getHostName (CharArray.toPoly buf, C_Size.fromInt n))
        in
          case CharArray.findi (fn (_, c) => c = #"\000") buf of
             NONE => CharArray.vector buf
           | SOME (i, _) =>
                CharArraySlice.vector (CharArraySlice.slice (buf, 0, SOME i))
        end

      fun scan reader state =
        let
          fun scanW state =
            case reader state of
              SOME (#"0", state') => 
                (case reader state' of
                    NONE => SOME (0w0, state')
                  | SOME (c, state'') =>
                     if Char.isDigit c
                        then StringCvt.wdigits StringCvt.OCT reader state'
                     else if c = #"x" orelse c = #"X"
                        then StringCvt.wdigits StringCvt.HEX reader state''
                     else SOME (0w0, state'))
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
            (Word8.fromLarge (Word.toLarge (Word.andb (w, 0wxFF))),
             Word.>>(w, 0w8))
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
                    then SOME (Vector.fromList [a,b,c,d], statew)
                    else NONE
                end
            | [(x, statex), (w, statew)] => 
                let
                  val (d,c,b,w) = get3 w
                  val (a,x) = get1 x
                in
                  if w = 0wx0 andalso x = 0wx0
                    then SOME (Vector.fromList [a,b,c,d], statew)
                    else try [(x, statex)]
                end
            | [(y, statey), (x, statex), (w, statew)] => 
                let
                  val (d,c,w) = get2 w
                  val (b,x) = get1 x
                  val (a,y) = get1 y
                in
                  if w = 0wx0 andalso x = 0wx0 andalso y = 0wx0
                    then SOME (Vector.fromList [a,b,c,d], statew)
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
                    then SOME (Vector.fromList [a,b,c,d], statew)
                    else try [(z, statez), (y, statey), (x, statex)]
                end
            | _ => NONE
        in
          try l
        end

      fun fromString s = StringCvt.scanString scan s
      fun toString in_addr =
        String.concatWith "." 
        (Vector.foldr (fn (w,ss) => (Word8.fmt StringCvt.DEC w)::ss) [] in_addr)
   end
