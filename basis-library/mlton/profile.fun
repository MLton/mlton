functor Profile (S:
		 sig
		    val isOn: bool
		    structure Data:
		       sig
			  type t (* = pointer *)
	       
			  val dummy: t
			  val free: t -> unit
			  val malloc: unit -> t
			  val reset: t -> unit
			  val write: t * word (* fd *) -> unit
		       end
		    val current: unit -> Data.t
		    val setCurrent: Data.t -> unit
		 end): sig
			  include MLTON_PROFILE
			  val cleanAtExit: unit -> unit
			  val cleanAtLoadWorld: unit -> unit
			  val init: unit -> unit
		       end =
struct

open S

structure Data =
   struct
      datatype t = T of {array: Data.t,
			 isCurrent: bool ref,
			 isFreed: bool ref}
   
      val all: t list ref = ref []
   
      local
	 fun make f (T r) = f r
      in
	 val array = make #array
	 val isCurrent = make #isCurrent
	 val isFreed = make #isFreed
      end

      fun equals (d, d') =
	 isFreed d = isFreed d'

      fun free (d as T {array, isCurrent, isFreed, ...}) =
	 if not isOn
	    then ()
	 else
	    if !isFreed
	       then raise Fail "free of freed profile data"
	    else if !isCurrent
		    then raise Fail "free of current profile data"
		 else
		    (all := List.foldl (fn (d', ac) =>
					if equals (d, d')
					   then ac
					else d' :: ac) [] (!all)
		     ; Data.free array
		     ; isFreed := true)

      fun make array =
	 T {array = array,
	    isCurrent = ref false,
	    isFreed = ref false}
	 
      fun malloc () =
	 let
	    val array =
	       if isOn
		  then Data.malloc ()
	       else Data.dummy
	    val d = make array
	    val _ = all := d :: !all
	 in
	    d
	 end

      fun reset (T {array, isFreed, ...}) =
	 if not isOn
	    then ()
	 else
	    if !isFreed
	       then raise Fail "reset of freed profile data"
	    else Data.reset array

      fun write (T {array, isFreed, ...}, file) =
	 if not isOn
	    then ()
	 else
	    if !isFreed
	       then raise Fail "write of freed profile data"
	    else
	       let
		  val fd =
		     let
			open Posix.FileSys
			open S
		     in
			creat (file,
			       flags [irusr, iwusr, irgrp, iwgrp, iroth, iwoth])
		     end
		  val _ = Data.write (array, Posix.FileSys.fdToWord fd)
		  val _ = Posix.IO.close fd
	       in
		  ()
	       end
   end

val r: Data.t ref = ref (Data.make S.Data.dummy)

fun current () = !r

fun setCurrent (d as Data.T {array, isCurrent, isFreed, ...}) =
   if not isOn
      then ()
   else
      if !isFreed
	 then raise Fail "setCurrent of freed profile data"
      else
	 let
	    val Data.T {isCurrent = ic, ...} = current ()
	    val _ = ic := false
	    val _ = isCurrent := true
	    val _ = r := d
	    val _ = S.setCurrent array
	 in
	    ()
	 end

fun init () = setCurrent (Data.make (S.current ()))

fun cleanAtExit () =
   let
      val _ = Data.write (current (), "mlmon.out")
      val _ = List.app (S.Data.free o Data.array) (!Data.all)
   in
      ()
   end

fun cleanAtLoadWorld () =
   let
      (* In a new world, all of the old profiling data is invalid. *)
      val _ = Data.all := []
   in
      ()
   end

end

