structure Tree: TREE =
struct

datatype 'a t = T of 'a * 'a t list
   
end
