functor RealX (S: REAL_X_STRUCTS): REAL_X = 
struct

open S

datatype t = T of {real: string,
		   size: RealSize.t}

local
   fun make f (T r) = f r
in
   val size = make #size
end

fun make (r, s) = T {real = r, size = s}

fun equals (T {real = r, ...}, T {real = r', ...}) = r = r'

fun toString (T {real = r, ...}) = r

val layout = Layout.str o toString

val hash = String.hash o toString

end
