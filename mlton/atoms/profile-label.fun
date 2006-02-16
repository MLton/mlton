(* Copyright (C) 2004-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ProfileLabel (S: PROFILE_LABEL_STRUCTS): PROFILE_LABEL =
   struct
      open S

      type int = Int.t
         
      datatype t = T of {plist: PropertyList.t,
                         uniq: int}

      local
         fun make f (T r) = f r
      in
         val plist = make #plist
         val uniq = make #uniq
      end

      local
         val c = Counter.new 0
      in
         fun new () = T {plist = PropertyList.new (),
                         uniq = Counter.next c}
      end

      fun toString (T {uniq, ...}) =
         concat ["MLtonProfile", Int.toString uniq]

      val layout = Layout.str o toString

      fun equals (l, l') = uniq l = uniq l'

      val clear = PropertyList.clear o plist
   end
