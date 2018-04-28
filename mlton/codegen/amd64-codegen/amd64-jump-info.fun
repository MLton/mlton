(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor amd64JumpInfo(S: AMD64_JUMP_INFO_STRUCTS) : AMD64_JUMP_INFO =
struct
  open S
  open amd64

  val tracer = amd64.tracer

  datatype status = Count of int | None

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
        val {get : Label.t -> status ref, ...}
          = Property.get(Label.plist, 
                         Property.initFun (fn _ => ref (Count 0)))
      in
        T {get = get}
      end

  local
    fun doit (status_ref, maybe_fn)
      = case !status_ref
          of None => ()
           | Count i => status_ref := (maybe_fn i)
  in
    fun incNear (T {get}, label)
      = doit (get label, fn i => Count (i+1))
    fun decNear (T {get}, label)
      = doit (get label, fn i => Count (i-1))
    fun forceNear (T {get}, label)
      = doit (get label, fn _ => None)
  end
  fun getNear (T {get}, label) = !(get label)

  fun completeJumpInfo {chunk = Chunk.T {blocks, ...},
                        jumpInfo: t}
    = List.foreach
      (blocks,
       fn Block.T {entry, transfer,...}
        => (case entry
              of Entry.Jump _ => ()
               | Entry.Func {label, ...} => forceNear (jumpInfo, label)
               | Entry.Cont {label, ...} => forceNear (jumpInfo, label)
               | Entry.Handler {label, ...} => forceNear (jumpInfo, label)
               | Entry.CReturn {label, func, ...}
               => if CFunction.maySwitchThreads func
                    then forceNear (jumpInfo, label)
                    else ();
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
        local
          val {get : Label.t -> status ref,
               destroy}
            = Property.destGet(Label.plist,
                               Property.initFun (fn _ => ref (Count 0)))
        in
          val jumpInfo' = T {get = get}
          val destroy = destroy
        end
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

        val _ = destroy ()
      in
        verified
      end

  val (verifyJumpInfo, verifyJumpInfo_msg)
    = tracer
      "verifyJumpInfo"
      verifyJumpInfo
end
