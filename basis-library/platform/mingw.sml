(* Copyright (C) 2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MinGW =
   struct
      fun getTempPath () =
         let
            fun lp bufSz =
               let
                  val buf = CharArray.arrayUninit (C_Size.toInt bufSz)
                  val reqSz = PrimitiveFFI.MinGW.getTempPath (bufSz, buf)
               in
                  if 0w0 = reqSz
                     then NONE
                  else if C_Size.< (reqSz, bufSz)
                     then SOME (CharArraySlice.vector
                                (CharArraySlice.unsafeSlice
                                 (buf, 0, SOME (C_Size.toInt reqSz))))
                  else lp reqSz
               end
         in
            (* Win32 MAX_PATH is 260, but some subsystems allow longer names *)
            lp 0w261
         end
   end
