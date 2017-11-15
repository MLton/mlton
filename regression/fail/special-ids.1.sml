signature SIG1 =
   sig
      datatype t = ref of unit
      exception true
      val false : int
   end
signature SIG2 =
   sig
      datatype t = it of unit
   end
signature SIG3 =
   sig
      exception it
   end
signature SIG4 =
   sig
      (* correct *)
      val it : int
   end

local
datatype t = op:: of unit | nil of unit
exception false
datatype u = it of unit
exception it
in end

local
fun ref z = z
in end
local
val rec ref = fn z => z
in end
local
fun true z = z
in end
local
val rec true = fn z => z
in end
local
fun op:: z = z
in end
local
val rec op:: = fn z => z
in end
local
(* correct *)
val rec it = fn z => z
in end
local
(* correct *)
fun it z = z
in end
