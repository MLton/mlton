(*kitreynolds3.sml*)

fun digit n = chr(ord #"0" + n)
fun digits(n,acc) =
      if n >=0 andalso n<=9 then digit n:: acc
      else digits (n div 10, digit(n mod 10) :: acc)

fun int_to_string(n) = implode(digits(n,[]))

fun rev l = (* linear-time reversal of lists! *)
  let fun loop([], acc) = acc
        | loop(x::xs, acc) = loop(xs, x::acc)
  in
     loop(l, [])
  end

fun foldR f b [] = b
  | foldR f b (x::xs) = f x (foldR f b xs)

fun curry f x y = f(x,y)

datatype 'a Option = None | Some of 'a 


datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree

fun max(i:int, j) = if i>j then i else j
fun member(x,[]) = false
  | member(x,x'::rest) = x=x' orelse member(x, rest)

fun search p Lf = false
  | search p (Br(x,t1,t2)) = 
      if member(x,p) then true
      else search (x::p) t1 orelse 
           search (x::p) t2

fun mk_tree 0 = Lf
  | mk_tree n = let val t = mk_tree(n-1)
                in Br(n,t,t)
                end
val it = if search [] (mk_tree 20)
        then print "true\n"
        else print "false\n";
