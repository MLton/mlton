structure MLtonFFI =
struct

local
  open MLtonThread
in
  fun handleCallFromC f =
    setCallFromCHandler (fn () => (atomicBegin();
				   f ();
				   atomicEnd()))
end
   
end
