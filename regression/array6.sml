(*
 Test various basis library functions.  Quite incomplete.
 
 This program should terminate without raising an exception and without
 printing anything if no bugs are discovered.
 If a bug is discovered, "assertion failed:" will be printed, followed
 by a string uniquely identifying the bug.
 *)

fun assert (msg,b) =
   ((*print (concat ["trying " ^ msg ^ "\n"]);*)
    if b then ()
    else print ("assertion failed: " ^ msg ^ "\n"))

(*------------------------------------------------------------------*)
(*                              Array                               *)
(*------------------------------------------------------------------*)

local
   open Array
   fun extract (arr, s, l) = ArraySlice.vector (ArraySlice.slice (arr, s, l))
   val copy = fn {src, si, len, dst, di} =>
      ArraySlice.copy {src = ArraySlice.slice (src, si, len),
                       dst = dst, di = di}
   fun appi f (arr, s, l) = 
      ArraySlice.appi (fn (i,x) => f (i+s,x)) (ArraySlice.slice (arr, s, l))

   val a0 = array (0,())
      
   val a1 = array (100,1)

   val a2 = fromList [0,1,2]

   val a3 = tabulate (13, fn x => x)
   val _ = update (a3,11,9)

   val v1 = extract (a3, 0, NONE)

   val v2 = extract (a3, 1, SOME 3)

   val a4 = array (10,47)
   val _ = copy {src = a3, si = 10, len = SOME 3,
                dst = a4, di = 1}

   val a5 = array (100, 0)
   val _ = appi (fn (i,_) => update (a5,i,i)) (a5, 0, NONE)
      
   val _ =
      List.app assert
      [("Array.length 0", length a0 = 0),
       ("Array.length 1", length a1 = 100),
       ("Array.length 2", length a2 = 3),
       ("Array.length 3", length a3 = 13),
       ("Array.sub 1", sub (a1, 50) = 1),
       ("Array.sub 2", sub (a2, 2) = 2),
       ("Array.sub 3a", sub (a3, 10) = 10),
       ("Array.sub 3b", sub (a3, 11) = 9),
       ("Vector.length 1", Vector.length v1 = 13),
       ("Vector.length 2", Vector.length v2 = 3),
       ("Vector.sub 1", sub (a4, 1) = sub (a3, 10)),
       ("Vector.sub 2", sub (a4, 2) = sub (a3, 11)),
       ("Vector.sub 3", sub (a4, 3) = sub (a3, 12)),
       ("Vector.sub 4", sub (a5, 50) = 50)]

   fun swap (a,i,j) =
      let val t = sub (a,i)
      in update (a, i, sub (a,j)) ;
         update (a, j, t)
      end
   
   fun bubbleSort (a, op <) =
      let val n = length a
         fun loop i =
            if i = n
               then ()
            else (let
                     fun loop j =
                        if j = 0
                           then ()
                        else if sub (a,j) < sub (a,j-1)
                                then (swap (a,j,j-1) ; loop (j-1))
                             else ()
                  in loop i
                  end ;
                     loop (i+1))
      in loop 0
      end

   fun isSorted (a, op <=) =
      let
         val max = length a - 1
         fun loop i =
            i = max orelse (sub (a, i) <= sub (a, i + 1)
                            andalso loop (i + 1))
      in loop 0
      end

   val size = 2000
   val a = tabulate (size, fn i => size - i)
   val _ = bubbleSort (a, op <)
   val _ = assert ("bubbleSort", isSorted (a, op <=))

in
end
