structure DynamicWind: DYNAMIC_WIND =
struct

fun wind(thunk, cleanup) =
   let val a = thunk()
   in cleanup(); a
   end handle exn => (cleanup(); raise exn)

end

