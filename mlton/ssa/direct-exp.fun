
functor DirectExp (S: DIRECT_EXP_STRUCTS): DIRECT_EXP =
struct
  
  open S

  type int = Int.t
  type word = Word.t

  open Exp Transfer

  structure Cont =
    struct
      type u = Label.t * Block.t list
      type t = Exp.t * Type.t -> u

      val layout = Layout.ignore

      fun nameGen (k: Var.t * Type.t -> u): t =
	 fn (e, ty) =>
	 case e of
	    Var x => k (x, ty)
	  | _ => let val x = Var.newNoname ()
	             val start = Label.newNoname ()
	             val (start', bs') = k (x, ty)
		 in (start,
		     Block.T {label = start,
			      args = Vector.new0 (),
			      statements = Vector.new1 (Statement.T {var = SOME x,
								     ty = ty,
								     exp = e}),
			      transfer = Goto {dst = start',
					       args = Vector.new0 ()}}
		     :: bs')
		 end

      fun name (k: Var.t -> u): t = nameGen (k o #1)

      val bug: t =
	 name (fn x =>
	       let
		  val start = Label.newNoname ()
	       in
		  (start,
		   [Block.T {label = start,
			     args = Vector.new0 (),
			     statements = Vector.new0 (),
			     transfer = Bug}])
	       end)

      fun goto (l: Label.t): t =
	 name (fn x =>
	       let
		  val start = Label.newNoname ()
	       in
		  (start,
		   [Block.T {label = start,
			     args = Vector.new0 (),
			     statements = Vector.new0 (),
			     transfer = Goto {dst = l, args = Vector.new1 x}}])
	       end)

      val return: t =
	 name (fn x =>
	       let
		  val start = Label.newNoname ()
	       in
		 (start,
		  [Block.T {label = start,
			    args = Vector.new0 (),
			    statements = Vector.new0 (),
			    transfer = Return (Vector.new1 x)}])
	       end)
			      
      val raisee: t =
	 name (fn x =>
	       let
		  val start = Label.newNoname ()
	       in
		 (start,
		  [Block.T {label = start,
			    args = Vector.new0 (),
			    statements = Vector.new0 (),
			    transfer = Raise (Vector.new1 x)}])
	       end)
			      
      fun finish (k: t, ety: Exp.t * Type.t): u = k ety
    end

  type u = Label.t * Block.t list
  type t = Cont.t -> u

  val layout = Layout.ignore


  fun sendCont (e: t, k: Cont.t): u = e k

  fun sendName (e: t, k: Var.t -> u): u = sendCont (e, Cont.name k)

  fun sendBug (e: t): u = sendCont (e, Cont.bug)

  fun sendGoto (e: t, l): u = sendCont (e, Cont.goto l)

  fun sendReturn (e: t): u = sendCont (e, Cont.return)


  fun convertsGen (es: t vector, k: Var.t vector -> u): u =
     let
        val n = Vector.length es
	fun loop (i, xs) =
	   if i = n
	      then k (Vector.fromListRev xs)
	   else sendName (Vector.sub (es, i), fn x => (loop (i+ 1, x :: xs)))
     in
        loop (0, [])
     end

  fun converts (es: t vector, make: Var.t vector -> Exp.t * Type.t): t =
     fn k => convertsGen (es, k o make)

  fun convert (e: t, make: Var.t -> Exp.t * Type.t): t =
     fn k => sendName (e, k o make)


  fun conApp {con, args, ty}: t =
     converts (args, fn args => (ConApp {con = con, 
					 args = args}, ty))

  fun const (c): t = 
     fn k => Cont.finish (k, (Const c, Type.ofConst c))

  fun primApp {prim, targs, args, ty}: t =
     let
        val primApp = 
	   converts (args, fn args => (PrimApp {prim = prim,
						targs = targs,
						args = args}, ty))
     in
        case Prim.name prim of
	   Prim.Name.MLton_halt => (fn _ => sendBug primApp)
	 | _ => primApp
     end

  fun select {tuple, offset, ty}: t = 
     convert (tuple, fn tuple => (Select {tuple = tuple, offset = offset}, ty))

  fun tuple (exps: t vector, ty: Type.t): t = 
     if Vector.length exps = 1
        then Vector.sub (exps, 0)
     else converts (exps, fn xs => (Exp.Tuple xs, ty))

  fun var (x, ty): t = 
     fn k => Cont.finish (k, (Var x, ty))


  fun name (e: t, make: Var.t -> t): t =
     fn k => sendName (e, fn x => sendCont (make x, k))

  fun seq (e1: t, e2: t): t =
     fn k => sendCont (e1, fn _ => sendCont (e2, k))


  fun call {func, args, ty}: t =
     fn k => 
     let
	val start = Label.newNoname ()
	val cont = Label.newNoname ()
        val result = Var.newNoname ()
	val (start', bs') = Cont.finish (k, (Var result, ty))
     in
        convertsGen 
	(args,
	 fn args => 
	 (start,
	  Block.T
	  {label = start,
	   args = Vector.new0 (),
	   statements = Vector.new0 (),
	   transfer = Call {func = func,
			    args = args,
			    return = (Return.NonTail
				      {cont = cont,
				       handler = Handler.None})}}
	  :: Block.T {label = cont,
		      args = Vector.new1 (result, ty),
		      statements = Vector.new0 (),
		      transfer = Goto {dst = start',
				       args = Vector.new0 ()}}
	  :: bs'))
     end

  datatype cases =
     Char of (char * t) vector
   | Con of {con: Con.t,
	     args: (Var.t * Type.t) vector,
	     body: t} vector
   | Int of (int * t) vector
   | Word of (word * t) vector
   | Word8 of (Word8.t * t) vector

  fun casee {test, cases, default, ty} =
     fn k =>
     let
        val start = Label.newNoname ()
	val join = Label.newNoname ()
        val result = Var.newNoname ()
	val (start',bs') = Cont.finish (k, (Var result, ty))
     in
        test (Cont.nameGen
	      (fn (test, testTy) =>
	       let
		  fun doCases (cases, finish, make) =
		     let
		        val (cases, bss) =
			   Vector.mapAndFold
			   (cases, [], fn (c, bss) =>
			    let
			       val (test, args, body) = make c
			       val l = Label.newNoname ()
			       val (start',bs') = sendGoto (body, join)
			    in
			       ((test, l),
				(Block.T {label = l,
					  args = args,
					  statements = Vector.new0 (),
					  transfer = Goto {dst = start',
							   args = Vector.new0 ()}}
				 :: bs')
				:: bss)
			    end)
		     in
		        (finish cases, List.concat bss)
		     end
		  fun doit (l, f) =
		     doCases (l, f, fn (test, body) => (test, Vector.new0 (), body))
		  val (cases, bsc) =
		     case cases of
		        Char l => doit (l, Cases.Char)
		      | Con l => 
			   doCases (l, Cases.Con, fn {con, args, body} =>
				    (con, args, body))
		      | Int l => doit (l, Cases.Int)
		      | Word l => doit (l, Cases.Word)
		      | Word8 l => doit (l, Cases.Word8)
		  val (default, bs) =
		     case default of
		        NONE => (NONE, bsc)
		      | SOME e => 
			   let
			      val (start', bs') = sendGoto (e, join)
			   in
			      (SOME start', bs' @ bsc)
			   end
	       in
		  (start,
		   Block.T {label = start,
			    args = Vector.new0 (),
			    statements = Vector.new0 (),
			    transfer = Case {test = test,
					     cases = cases,
					     default = default}} 
		   :: Block.T {label = join,
			       args = Vector.new1 (result, ty),
			       statements = Vector.new0 (),
			       transfer = Goto {dst = start', 
						args = Vector.new0 ()}}
		   :: (bs @ bs'))
	       end))
     end

  local
     fun make c = conApp {con = c, args = Vector.new0 (), ty = Type.bool}
  in
     val truee = make Con.truee
     val falsee = make Con.falsee
  end

  val int = const o Const.fromInt
     
  fun eq (e1, e2, ty) =
     primApp {prim = Prim.eq,
	      targs = Vector.new1 ty,
	      args = Vector.new2 (e1, e2),
	      ty = Type.bool}

end
