functor Ffi (S: FFI_STRUCTS): FFI = 
struct

open S

structure Type =
   struct
      datatype t =
	 Bool
       | Char
       | Int of IntSize.t
       | Pointer
       | Real of RealSize.t
       | Word of WordSize.t

      fun memo (f: t -> 'a): t -> 'a =
	 let
	    val bool = f Bool
	    val char = f Char
	    val int = IntSize.memoize (f o Int)
	    val pointer = f Pointer
	    val real = RealSize.memoize (f o Real)
	    val word = WordSize.memoize (f o Word)
	 in
	    fn Bool => bool
	     | Char => char
	     | Int s => int s
	     | Pointer => pointer
	     | Real s => real s
	     | Word s => word s
	 end

      val toString =
	 memo
	 (fn u =>
	  case u of
	     Bool => "Bool"
	   | Char => "Char"
	   | Int s => concat ["Int", IntSize.toString s]
	   | Pointer => "Pointer"
	   | Real s => concat ["Real", RealSize.toString s]
	   | Word s => concat ["Word", WordSize.toString s])
   end

val exports: {args: Type.t vector,
	      id: int,
	      name: string,
	      res: Type.t} list ref = ref []

fun numExports () = List.length (!exports)

local
   val exportCounter = Counter.new 0
in
   fun addExport {args, name, res} =
      let
	 val id = Counter.next exportCounter
	 val _ = List.push (exports, {args = args,
				      id = id,
				      name = name,
				      res = res})
      in
	 id
      end
end

val headers: string list ref = ref []

fun declareHeaders {print} =
   List.foreach (!headers, fn s => (print s; print ";\n"))
       
fun declareExports {print} =
   let
      val maxMap = Type.memo (fn _ => ref ~1)
      fun bump (t, i) =
	 let
	    val r = maxMap t
	 in
	    r := Int.max (!r, i)
	 end
      val _ =
	 List.foreach
	 (!exports, fn {args, res, ...} =>
	  let
	     val map = Type.memo (fn _ => Counter.new 0)
	  in
	     Vector.foreach (args, fn t => bump (t, Counter.next (map t)))
	     ; bump (res, 0)
	  end)
      (* Declare the arrays and functions used for parameter passing. *)
      val _ =
	 Type.memo
	 (fn t =>
	  let
	     val n = !(maxMap t)
	  in
	     if n >= 0
		then
		   let
		      val size = Int.toString (1 + n)
		      val t = Type.toString t
		   in
		      print (concat [t, " MLton_FFI_", t, "[", size, "];\n"])
		      ; print (concat [t, " MLton_FFI_get", t, " (Int i) {\n",
				       "\treturn MLton_FFI_", t, "[i];\n",
				       "}\n"])
		      ; print (concat
			       [t, " MLton_FFI_set", t, " (", t, " x) {\n",
				"\tMLton_FFI_", t, "[0] = x;\n",
				"}\n"])
		   end
	     else ()
	  end)
      val _ = print "Int MLton_FFI_op;\n"
      val _ = print (concat ["Int MLton_FFI_getOp () {\n",
			     "\treturn MLton_FFI_op;\n",
			     "}\n"])
   in
      List.foreach
      (!exports, fn {args, id, name, res} =>
       let
	  val varCounter = Counter.new 0
	  val map = Type.memo (fn _ => Counter.new 0)
	  val args =
	     Vector.map
	     (args, fn t =>
	      let
		 val index = Counter.next (map t)
		 val x = concat ["x", Int.toString (Counter.next varCounter)]
		 val t = Type.toString t
	      in
		 (x,
		  concat [t, " ", x],
		  concat ["\tMLton_FFI_", t, "[", Int.toString index, "] = ",
			  x, ";\n"])
	      end)
	  val header =
	     concat [Type.toString res,
		     " ", name, " (",
		     concat (List.separate (Vector.toListMap (args, #2), ", ")),
		     ")"]
	  val _ = List.push (headers, header)
       in
	  print (concat [header, " {\n"])
	  ; print (concat ["\tMLton_FFI_op = ", Int.toString id, ";\n"])
	  ; Vector.foreach (args, fn (_, _, set) => print set)
	  ; print ("\tMLton_callFromC ();\n")
	  ; print (concat ["\treturn MLton_FFI_", Type.toString res, "[0];\n"])
	  ; print "}\n"
       end)
   end

end
