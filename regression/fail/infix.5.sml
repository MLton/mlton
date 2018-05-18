infix  1  <!  <@
infix  2 <!! <@@
infixr 1  !>  @>
infixr 2 !!> @@>

infix 0 &
infix 9 &&

datatype ('a, 'b) t1 = & of 'a * 'b
datatype ('a, 'b) t2 = && of 'a * 'b

(* defines <! *)
local
   fun op <! (x, y) = ()
in
   val _ = op <!
end

(* defines <! *)
local
   fun op <! (x, y) z = ()
in
   val _ = op <!
end

(* error *)
local
   fun (op <!) (x, y) = ()
in
end

(* error *)
local
   fun (op <!) (x, y) z = ()
in
end

(* error? *)
local
   fun (op <! (x, y)) = ()
in
end

(* error? *)
local
   fun (op <! (x, y)) z = ()
in
end

(* error *)
local
   fun <! (x, y) = ()
in
end

(* error *)
local
   fun <! (x, y) z = ()
in
end

(* error *)
local
   fun (<! (x, y)) = ()
in
end

(* error *)
local
   fun (<! (x, y)) z = ()
in
end
