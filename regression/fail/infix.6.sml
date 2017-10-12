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
   fun x <! y = ()
in
   val _ = op <!
end

(* error *)
local
   fun x <! y z = ()
in
end

(* defines <! *)
local
   fun (x <! y) = ()
in
   val _ = op <!
end

(* defines <! *)
local
   fun (x <! y) z = ()
in
   val _ = op <!
end

(* error *)
local
   fun <! <@ y = ()
in
end

(* error *)
local
   fun <! <@ y z = ()
in
end

(* error *)
local
   fun <! <@ y = ()
in
end

(* error *)
local
   fun a <! <@ = ()
in
end

(* error *)
local
   fun (a <! <@) = ()
in
end

(* defines <@ *)
local
   fun op <! <@ y = ()
in
   val _ = op <@
end

(* error *)
local
   fun op <! <@ y z = ()
in
end

(* defines <@ *)
local
   fun (op <! <@ y) z = ()
in
   val _ = op <@
end

(* error *)
local
   fun <! op <@ y = ()
in
end

(* error *)
local
   fun (<! op <@ y) = ()
in
end

(* defines <@ *)
local
   fun op <! op <@ y = ()
in
   val _ = op <!
end

(* defines <@ *)
local
   fun op <! op <@ y z = ()
in
   val _ = op <!
end
