(* Copyright (C) 2009 Matthew Fluet.
 * Copyright (C) 2004-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor ProfileLabel (S: PROFILE_LABEL_STRUCTS): PROFILE_LABEL =
   struct
      open S

      datatype t = T of {plist: PropertyList.t,
                         uniq: int}

      local
         fun make f (T r) = f r
      in
         val plist = make #plist
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

      val clear = PropertyList.clear o plist
   end
