structure MLtonFFI =
struct

val handleCallFromC = fn f => MLtonThread.setCallFromCHandler (true, f)
   
end
