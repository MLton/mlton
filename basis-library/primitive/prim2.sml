(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure Primitive =
   struct
      open Primitive

      structure MLton =
         struct
            open MLton
            val bug = PrimitiveFFI.MLton.bug
         end

      val dontInline: (unit -> 'a) -> 'a =
         fn f =>
         let
            val rec recur: Int32.int -> 'a =
               fn i =>
               if i = 0
                  then f ()
                  else let
                          val _ = recur (Int32.- (i, 1))
                       in
                          recur (Int32.- (i, 2))
                       end
         in
            recur 0
         end
   end

(* Install an emergency exception handler. *)
local
   structure P = Primitive
   structure PFFI = PrimitiveFFI
   val _ =
      P.TopLevel.setHandler 
      (fn exn => 
       (PFFI.Stdio.print "unhandled exception: "
        ; case exn of
             P.Exn.Fail8 msg => (PFFI.Stdio.print "Fail "
                                 ; PFFI.Stdio.print msg)
           | _ => PFFI.Stdio.print (P.Exn.name exn)
        ; PFFI.Stdio.print "\n"
        ; P.MLton.bug (P.NullString8.fromString 
                       "unhandled exception in Basis Library\000")))
in
end

(* Install an emergency suffix. *)
local
   structure P = Primitive
   val _ =
      P.TopLevel.setSuffix
      (fn () => 
       (P.MLton.halt 0
        ; P.MLton.bug (P.NullString8.fromString 
                       "missing suffix in Basis Library\000")))
in
end
