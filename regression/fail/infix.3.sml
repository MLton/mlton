infix  1  <!  <@
infix  2 <!! <@@
infixr 1  !>  @>
infixr 2 !!> @@>

infix 0 &
infix 9 &&

datatype ('a, 'b) t1 = & of 'a * 'b
datatype ('a, 'b) t2 = && of 'a * 'b

(* error *)
local
   fun () = ()
in
end

(* error *)
local
   fun () _ = ()
in
end

(* error *)
local
   fun () _ _ = ()
in
end

(* error *)
local
   fun () _ _ _ = ()
in
end

(* error *)
local
   fun (f) = ()
in
end

(* error *)
local
   fun (f) _ = ()
in
end

(* error *)
local
   fun (f) _ _ = ()
in
end

(* error *)
local
   fun (f) _ _ _ = ()
in
end
