(* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 2002-2006, 2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Net : NET =
   struct
      structure AddrFamily = MkAbsRepEq(type rep = C_Int.t)
      structure Sock = MkAbsRep(type rep = C_Sock.t)
      structure SockType = MkAbsRepEq(type rep = C_Int.t)

      structure Prim = PrimitiveFFI.Net

      structure Word32 =
         struct
            val hton = Prim.htonl
            val ntoh = Prim.ntohl
         end
      structure Word16 =
         struct
            val hton = Prim.htons
            val ntoh = Prim.ntohs
         end

      structure Int32 =
         struct
            val hton = 
               Primitive.IntWordConv.idFromWord32ToInt32 
               o Word32.hton 
               o Primitive.IntWordConv.idFromInt32ToWord32
            val ntoh = 
               Primitive.IntWordConv.idFromWord32ToInt32 
               o Word32.ntoh 
               o Primitive.IntWordConv.idFromInt32ToWord32
         end
      structure Int16 =
         struct
            val hton = 
               Primitive.IntWordConv.idFromWord16ToInt16 
               o Word16.hton 
               o Primitive.IntWordConv.idFromInt16ToWord16
            val ntoh = 
               Primitive.IntWordConv.idFromWord16ToInt16 
               o Word16.ntoh 
               o Primitive.IntWordConv.idFromInt16ToWord16
         end

      structure C_Int =
         struct
            local
               structure S =
                  C_Int_ChooseIntN
                  (type 'a t = 'a -> 'a
                   val fInt8 = fn _ => raise Fail "Net.C_Int.hton: fInt8"
                   val fInt16 = Int16.hton
                   val fInt32 = Int32.hton
                   val fInt64 = fn _ => raise Fail "Net.C_Int.hton: fInt64")
            in
               val hton = S.f
            end
            local
               structure S =
                  C_Int_ChooseIntN
                  (type 'a t = 'a -> 'a
                   val fInt8 = fn _ => raise Fail "Net.C_Int.ntoh: fInt8"
                   val fInt16 = Int16.ntoh
                   val fInt32 = Int32.ntoh
                   val fInt64 = fn _ => raise Fail "Net.C_Int.ntoh: fInt64")
            in
               val ntoh = S.f
            end
         end
   end
