functor Profile
   (structure Cleaner:
       sig
          type t

          val addNew: t * (unit -> unit) -> unit
          val atExit: t
       end
    structure Profile:
       sig
          val profile: bool
             
          structure Data:
             sig
                type t (* = pointer *)

		val dummy: t
                val free: t -> unit
                val malloc: unit -> t
                val reset: t -> unit
                val write: t * word (* fd *) -> unit
             end
	  val init: unit -> unit
	  val installHandler: unit -> unit
          val setCurrent: Data.t -> unit
       end): MLTON_PROFILE =
struct

val profile = Profile.profile

structure Data =
   struct
      datatype t = T of {array: Profile.Data.t,
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
	 if not profile
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
		     ; Profile.Data.free array
		     ; isFreed := true)

      fun malloc () =
	 let
	    val array =
	       if profile
		  then Profile.Data.malloc ()
	       else Profile.Data.dummy
	    val d = T {array = array,
		       isCurrent = ref false,
		       isFreed = ref false}
	    val _ = all := d :: !all
	 in
	    d
	 end

      fun reset (T {array, isFreed, ...}) =
	 if not profile
	    then ()
	 else
	    if !isFreed
	       then raise Fail "reset of freed profile data"
	    else Profile.Data.reset array

      fun write (T {array, isFreed, ...}, file) =
	 if not profile
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
		  val _ = Profile.Data.write (array, Posix.FileSys.fdToWord fd)
		  val _ = Posix.IO.close fd
	       in
		  ()
	       end
   end

fun setItimer (t: Time.time): unit =
   Itimer.set (Itimer.Prof, {interval = t, value = t})

val _ =
   if not profile
      then ()
   else Profile.init ()

val initData = Data.malloc ()

val r = ref initData

fun current () = !r

fun setCurrent (d as Data.T {array, isCurrent, isFreed, ...}) =
   if not profile
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
	    val _ = Profile.setCurrent array
	 in
	    ()
	 end
   
val _ =
   if not profile
      then ()
   else
      let
	 val _ = setCurrent initData
	 val _ = Profile.installHandler ()
	 val _ = setItimer (Time.fromMilliseconds 10)
	 val _ =
	    Cleaner.addNew
	    (Cleaner.atExit, fn () =>
	     let
		val _ = setItimer Time.zeroTime
		val _ = Data.write (current (), "mlmon.out")
		val _ = List.app (Profile.Data.free o Data.array) (!Data.all)
	     in
		()
	     end)
      in
	 ()
      end

end

