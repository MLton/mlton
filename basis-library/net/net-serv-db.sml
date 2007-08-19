(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure NetServDB: NET_SERV_DB =
   struct
      structure Prim = PrimitiveFFI.NetServDB

      datatype entry = T of {name: string,
                             aliases: string list,
                             port: C_Int.t,
                             protocol: string}

      local
        fun make s (T r) = s r
      in
        val name = make #name
        val aliases = make #aliases
        val port = C_Int.toInt o (make #port)
        val protocol = make #protocol
      end

      local
        fun get (i: C_Int.t): entry option =
          if i <> C_Int.zero
            then let
                   val name = CUtil.C_String.toString (Prim.getEntryName ())
                   val numAliases = Prim.getEntryAliasesNum ()
                   fun fill (n, aliases) =
                     if C_Int.< (n, numAliases)
                       then let
                              val alias = CUtil.C_String.toString (Prim.getEntryAliasesN n)
                            in
                              fill (C_Int.+ (n, 1), alias::aliases)
                            end
                       else List.rev aliases
                   val aliases = fill (0, [])
                   val port = Net.C_Int.ntoh (Prim.getEntryPort ())
                   val protocol = CUtil.C_String.toString (Prim.getEntryProto ())
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
            val port = Net.C_Int.hton (C_Int.fromInt port)
          in
            case proto of
               NONE => get (Prim.getByPortNull port)
             | SOME proto =>
                  get (Prim.getByPort (port, NullString.nullTerm proto))
          end
      end
   end
