
local
  val isNormal = Real.isNormal
  val isFinite = Real.isFinite
  (*
   * Total hack!!  Bounce r through isNormal (a C call) to force r out of the FPU.
   * Interestingly, the same hack works for SML/NJ.
   *)
  val isPositive = fn r => (Real.isNormal r; r > 0.0)

  fun min (p: real -> bool): real =
    let
      fun loop (x: real): real =
	let
	  val y = x / 2.0
	in
	  if p y
	    then loop y
	    else x
	end
    in loop 1.0
	    end
in
  val minNormalPos = min isNormal
  val minPos = min isPositive
    
  val maxFinite =
    let
      fun up (x: real): real =
	let
	  val y = x * 2.0
	in
	  if isFinite y
	    then up y
	    else x
	end
      fun down (x: real, y: real): real =
		  let
		    val y = y / 2.0
		    val z = x + y
		  in
		    if isFinite z
		      then down (z, y)
		      else x
		  end
      val z = up 1.0
	    in
	      down (z, z)
    end
end

val _ = print ((Real.toString maxFinite) ^ "\n")
val _ = print ((Real.toString Real.maxFinite) ^ "\n")
val _ = print ((Bool.toString (Real.==(Real.maxFinite, maxFinite))) ^ "\n")
val _ = print ((Real.toString minPos) ^ "\n")
val _ = print ((Real.toString Real.minPos) ^ "\n")
val _ = print ((Bool.toString (Real.==(Real.minPos, minPos))) ^ "\n")
val _ = print ((Real.toString minNormalPos) ^ "\n")
val _ = print ((Real.toString Real.minNormalPos) ^ "\n")
val _ = print ((Bool.toString (Real.==(Real.minNormalPos, minNormalPos))) ^ "\n")
