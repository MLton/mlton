signature MLTON_CALLBACK =
  sig
    structure Type :
      sig
	type ('a, 'b) arg
	type 'a res

	val C: (char, 'b) arg
	val B: (bool, 'b) arg
	val I: (int, 'b) arg
	val R: (real, 'b) arg
	val U: (unit, 'b) arg
	val W: (word, 'b) arg
	  
	val --> : ('a, 'b) arg * 'b res -> ('a -> 'b) res
	  
	val C' : char res
	val B' : bool res
	val I' : int res
	val R': real res
	val U' : unit res
	val W' : word res
      end

    val register: string * ('a -> 'b) Type.res -> ('a -> 'b) -> unit
    val unregister: string -> unit
    val isRegistered: string -> bool
  end

signature MLTON_CALLBACK_EXTRA =
  sig
    include MLTON_CALLBACK
  end