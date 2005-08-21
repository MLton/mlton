signature RING =
   sig
      type ring
      type elt

      val make : {zero : 'a,
                  one : 'a,
                  + : 'a * 'a -> 'a,
                  * : 'a * 'a -> 'a} -> {ring : ring,
                                         valOf : elt -> 'a}
         
      val zero : ring -> elt
      val one : ring -> elt
      val ringOf : elt -> ring

      exception TypeError (* raised by * or + with bogus args *)
      val * : elt * elt -> elt
      val + : elt * elt -> elt
   end

structure Ring : RING =
   struct
      datatype ring =
         Ring of unit -> {zero : elt,
                          one : elt,
                          + : elt * elt -> elt,
                          * : elt * elt -> elt}
      and elt = Elt of unit -> {ring : ring}

      fun ringOf(Elt th) = #ring(th())

      fun extract sel (Ring th) = sel(th())

      val zero = extract #zero
      val one = extract #one

      local
         fun make sel (x,y) = extract sel (ringOf x) (x,y)
      in
         val op * = make(# * )
         val op + = make(# +)
      end

      exception TypeError
      
      fun 'a make{zero, one, +, * = op *} =
         let
            val r : 'a option ref = ref NONE

            fun valOf(Elt th) =
               (th() ;
                case !r of
                   NONE => raise TypeError
                 | SOME x => (x before r := NONE))
                      
            fun ring() = {zero = elt zero,
                          one = elt one,
                          + = binary(op +),
                          * = binary(op * )}
            and elt(x : 'a) =
               Elt(fn () => (r := SOME x ;
                             {ring = Ring ring}))
            and binary (f : 'a * 'a -> 'a) (x : elt, y : elt) =
               elt(f(valOf x, valOf y))
               
         in
            {ring = Ring ring,
             valOf = valOf}
         end
   end

val {ring = ints, valOf} = Ring.make{zero = 0,
                                     one = 1,
                                     + = op +,
                                     * = op *}

val _ = (print(Int.toString(valOf(Ring.+(Ring.one ints,
                                         Ring.one ints)))) ;
         print "\n")
