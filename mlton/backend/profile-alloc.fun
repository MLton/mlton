(* Copyright (C) 2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)

functor ProfileAlloc (S: PROFILE_ALLOC_STRUCTS): PROFILE_ALLOC = 
struct

open S
open Rssa

structure CFunction =
   struct
      open CFunction

      val profileAllocInc =
	 T {bytesNeeded = NONE,
	    ensuresBytesFree = false,
	    modifiesFrontier = false,
	    modifiesStackTop = false,
	    mayGC = false,
	    maySwitchThreads = false,
	    name = "MLton_ProfileAlloc_inc",
	    needsProfileAllocIndex = true,
	    returnTy = NONE}
   end

fun doit (Program.T {functions, main, ...}) =
   let
      (* Start the counter at 1 because element 0 is PROFILE_ALLOC_MISC. *)
      val counter = Counter.new 1
      val profileAllocLabels = ref []
      val labelIndex = String.memoize (fn s =>
				       (List.push (profileAllocLabels, s)
					; Counter.next counter))
      fun doFunction (f: Function.t) =
	 let
	    val {args, blocks, name, start} = Function.dest f
	    val extraBlocks = ref []
	    val blocks =
	       Vector.map
	       (blocks,
		fn block as Block.T {args, kind, label, profileInfo, statements,
				     transfer} =>
		let
		   val objectsSize =
		      Vector.fold
		      (statements, 0, fn (s, ac) =>
		       case s of
			  Statement.Object {numPointers, numWordsNonPointers,
					    ...} =>
			     ac
			     + Runtime.normalHeaderSize
			     + (Runtime.normalSize
				{numPointers = numPointers,
				 numWordsNonPointers = numWordsNonPointers})
			| Statement.PrimApp {prim, ...} =>
			    (case Prim.name prim of
				Prim.Name.Array_array0 => ac + Runtime.array0Size
			      | _ => ac)
			| _ => ac)
		   val needs =
		      case transfer of
			 Transfer.CCall {func, ...} =>
			    CFunction.needsProfileAllocIndex func
		       | _ => false
		   val statements =
		      if objectsSize > 0 orelse needs
			 then
			    Vector.concat
			    [Vector.new1
			     (Statement.Move
			      {dst = (Operand.Runtime
				      Runtime.GCField.ProfileAllocIndex),
			       src = (Operand.word
				      (Word.fromInt
				       (labelIndex
					(#label (#ssa profileInfo)))))}),

			     statements]
		      else statements
		in
		   if objectsSize = 0
		      then Block.T {args = args,
				    kind = kind,
				    label = label,
				    profileInfo = profileInfo,
				    statements = statements,
				    transfer = transfer}
		   else
		      let
			 val newLabel = Label.newNoname ()
			 val func = CFunction.profileAllocInc
			 val _ =
			    List.push
			    (extraBlocks,
			     Block.T {args = Vector.new0 (),
				      kind = Kind.CReturn {func = func},
				      label = newLabel,
				      profileInfo = profileInfo,
				      statements = Vector.new0 (),
				      transfer = transfer})
			 val transfer =
			    Transfer.CCall
			    {args = (Vector.new1
				     (Operand.word (Word.fromInt objectsSize))),
			     func = func,
			     return = SOME newLabel}
		      in
			 Block.T {args = args,
				  kind = kind,
				  label = label,
				  profileInfo = profileInfo,
				  statements = statements,
				  transfer = transfer}
		      end
		end)
	    val blocks = Vector.concat [blocks,
					Vector.fromList (!extraBlocks)]
	 in
	    Function.new {args = args,
			  blocks = blocks,
			  name = name,
			  start = start}
	 end
      val functions = List.revMap (functions, doFunction)
      val main = doFunction main
      val profileAllocLabels = Vector.fromListRev (!profileAllocLabels)
   in
      Program.T {functions = functions,
		 main = main,
		 profileAllocLabels = profileAllocLabels}
   end

end
