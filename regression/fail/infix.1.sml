(* error *)
local
   fun _ f _ = ()
in
end

(* error *)
local
   fun (_ f _) = ()
in
end

(* error *)
local
   fun (_ f _) _ = ()
in
end

infix && ||

(* error *)
local
   fun && x = ()
in
end

(* error *)
local
   fun x && = ()
in
end

(* error *)
local
   fun && || = ()
in
end

(* error *)
local
   fun || && = ()
in
end
