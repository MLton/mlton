local
   val 'a rec f = fn () => ()
in
end

local
   val x = "Hello, world!\n"
      
   val x = "BUG\n"
   and rec f = fn () => print x
in
   val _ = f ()
end

local
   val x = "BUG\n"

   val x = "Hello, world!\n"
   and rec f = fn y => print y
in
   val _ = f x
end

local
   val rec rec f = fn () => ()
in
end

local
   val rec f = fn () => ()
   and rec g = fn () => ()
in
end
