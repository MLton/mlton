functor PropertyList (H: HET_CONTAINER):> PROPERTY_LIST =
struct

datatype t = T of H.t list ref

fun new (): t = T (ref [])

fun length (T r) = List.length (!r)

val equals = fn (T r, T r') => Ref.equals (r, r')

fun clear (T hs) = hs := []

val numPeeks: int ref = ref 0
val numLinks: int ref = ref 0
val maxLength: int ref = ref 0
   
fun stats () =
   let open Layout
   in align
      [seq [str "numPeeks = ", Int.layout (!numPeeks)],
       seq [str "maxLength = ", Int.layout (!maxLength)],
       seq [str "average position in property list = ",
	    str let open Real
		in format (fromInt (!numLinks) / fromInt (!numPeeks),
			   Format.fix (SOME 3))
		end]]
   end

fun print s = TextIO.output (TextIO.stdErr, s)

fun 'a newProperty () =
   let
      val {make, pred, peek = peekH} = H.new ()
      fun peek (T hs) =
	 let
	    fun loop (l, n) =
	       let
		  fun update () = (numLinks := n + !numLinks;
				   maxLength := Int.max(!maxLength, n))
	       in case l of
		  [] => (update (); NONE)
		| e :: l =>
		     case peekH e of
			r as SOME _ => (update (); r)
		      | NONE => loop (l, n + 1)
	       end
	 in numPeeks := 1 + !numPeeks
	    ; loop (!hs, 0)
	 end

      fun get (plist: t) =
	 case peek plist of
	    SOME x => x
	  | NONE => Error.bug "get"

      fun set (T hs, v: 'a, f: unit -> unit): unit =
	 let val h = make v
	    val rec loop =
	       fn [] => (f (); [h])
		| h' :: hs => if pred h'
				 then h :: hs
			      else h' :: loop hs
	 in hs := loop (!hs)
	 end

      fun add (T hs, v: 'a): unit = hs := make v :: !hs

      fun remove (T hs) = hs := List.remove (!hs, pred)
   in
      {add = add, peek = peek, get = get, set = set, remove = remove}
   end

end
