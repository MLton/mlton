functor ProfileLabel (S: PROFILE_LABEL_STRUCTS): PROFILE_LABEL =
   struct
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
