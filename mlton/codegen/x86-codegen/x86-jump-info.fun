
functor x86JumpInfo(S: X86_JUMP_INFO_STRUCTS) : X86_JUMP_INFO =
struct
  open S
  open x86

  val tracer = x86.tracer
    
  datatype status = Count of int | None

  val status_lt
    = fn (Count i1, None) => true
       | (Count i1, Count i2) => i1 < i2
       | _ => false
  val status_eq
    = fn (None    , None    ) => true
       | (Count i1, Count i2) => i1 = i2
       | _ => false

  val status_toString
    = fn None => "None"
       | Count i => concat ["Count ", Int.toString i]

  datatype t = T of {get: Label.t -> status ref}
      
  fun newJumpInfo ()
    = let
	val jumpInfo as {get : Label.t -> status ref}
	  = Property.get(Label.plist, 
			 Property.initFun (fn _ => ref (Count 0)))
      in
	T jumpInfo
      end

  fun resetNear (jumpInfo as T {get}, label) = (get label) := Count 0
  local
    fun doit (status_ref, maybe_fn)
      = case !status_ref
	  of None => ()
	   | Count i => status_ref := (maybe_fn i)
  in
    fun incNear (jumpInfo as T {get}, label)
      = doit (get label, fn i => Count (i+1))
    fun decNear (jumpInfo as T {get}, label)
      = doit (get label, fn i => Count (i-1))
    fun forceNear (jumpInfo as T {get}, label)
      = doit (get label, fn i => None)
  end
  fun getNear (jumpInfo as T {get}, label) = !(get label)

  fun completeJumpInfo {chunk = Chunk.T {blocks, ...},
			jumpInfo: t}
    = List.foreach
      (blocks,
       fn Block.T {entry, transfer,...}
        => (case entry
	      of Entry.Jump {label, ...} => ()
	       | Entry.Func {label, ...} => forceNear (jumpInfo, label)
	       | Entry.Cont {label, ...} => forceNear (jumpInfo, label)
	       | Entry.Handler {label, ...} => forceNear (jumpInfo, label)
	       | Entry.Runtime {label, ...} => forceNear (jumpInfo, label)
	       | Entry.CReturn {label, ...} => ();
	    List.foreach
	    (Transfer.nearTargets transfer,
	     fn label 
	      => incNear (jumpInfo, label))))

  val (completeJumpInfo, completeJumpInfo_msg)
    = tracer
      "completeJumpInfo"
      completeJumpInfo

  fun verifyJumpInfo {chunk as Chunk.T {blocks, ...}, 
		      jumpInfo: t}
    = let
	val jumpInfo' = newJumpInfo ()
	val _ = completeJumpInfo {chunk = chunk,
				  jumpInfo = jumpInfo'}

	val verified 
	  = List.forall
	    (blocks,
	     fn Block.T {entry,...}
	      => let
		   val label = Entry.label entry
		 in 
		   if status_eq(getNear(jumpInfo, label), 
				getNear(jumpInfo', label))
		     then true
		     else (print "verifyJumpInfo: ";
			   print (Label.toString label);
			   print "\n";
			   print "jumpInfo: ";	
			   print (status_toString (getNear(jumpInfo, label)));
			   print "\n";
			   print "jumpInfo': ";
			   print (status_toString (getNear(jumpInfo', label)));
			   print "\n";
			   false)
		 end)
      in
	verified
      end

  val (verifyJumpInfo, verifyJumpInfo_msg)
    = tracer
      "verifyJumpInfo"
      verifyJumpInfo
end

