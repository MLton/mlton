(* Copyright (C) 2002-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure NetProtDB: NET_PROT_DB =
   struct
      structure Prim = PrimitiveFFI.NetProtDB

      datatype entry = T of {name: string,
                             aliases: string list,
                             protocol: C_Int.t}

      local
        fun make s (T r) = s r
      in
        val name = make #name
        val aliases = make #aliases
        val protocol = C_Int.toInt o (make #protocol)
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
                   val protocol = Prim.getEntryProto ()
                 in
                   SOME (T {name = name,
                            aliases = aliases,
                            protocol = protocol})
                 end
            else NONE
      in
        fun getByName name = 
          get (Prim.getByName (NullString.nullTerm name))
        fun getByNumber proto = 
          get (Prim.getByNumber (C_Int.fromInt proto))
      end
   end
