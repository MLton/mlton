(* Copyright (C) 2009-2010 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor PolyHash (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S

(*
 * This pass implements polymorphic, structural hashing.
 *
 * For each datatype tycon and vector type, it builds a hashing function and
 * translates calls to MLton_hash into calls to that function.
 *
 * For tuples, it does the hashing inline.  I.E. it does not create
 * a separate hashing function for each tuple type.
 *
 * All hashing functions are only created if necessary, i.e. if hashing
 * is actually used at a type.
 *
 * Optimizations:
 *)

open Exp Transfer

structure Dexp =
   struct
      open DirectExp

      fun wordFromWord (w: word, ws: WordSize.t): t =
         word (WordX.fromIntInf (Word.toIntInf w, ws))

      fun shiftInt i =
         word (WordX.fromIntInf (i, WordSize.shiftArg))
      fun shiftBits b = shiftInt (Bits.toIntInf b)

      local
         fun mk prim =
            fn (e1: t, e2: t, s) =>
            primApp {prim = prim s,
                     targs = Vector.new0 (),
                     args = Vector.new2 (e1, e2),
                     ty = Type.word s}
      in
         val add = mk Prim.wordAdd
         val andb = mk Prim.wordAndb
         val rshift = mk (fn s => Prim.wordRshift (s, {signed = false}))
         val xorb = mk Prim.wordXorb
      end
      local
         fun mk prim =
            fn (e1: t, e2: t, s, sg) =>
            primApp {prim = prim (s, sg),
                     targs = Vector.new0 (),
                     args = Vector.new2 (e1, e2),
                     ty = Type.word s}
      in
         val mul = mk Prim.wordMul
      end

      fun wordEqual (e1: t, e2: t, s): t =
         primApp {prim = Prim.wordEqual s,
                  targs = Vector.new0 (),
                  args = Vector.new2 (e1, e2),
                  ty = Type.bool}
end

structure Hash =
   struct
      val resWordSize = WordSize.word32
      val resTy = Type.word resWordSize

      fun mkWordBytes {stateTy: Type.t,
                       workWordSize: WordSize.t,
                       combByte: Dexp.t * Dexp.t -> Dexp.t,
                       mix: Dexp.t -> Dexp.t} =
         let
            val workBits = WordSize.bits workWordSize
            val workTy = Type.word workWordSize
            fun wordBytes (st,w,ws) =
               let
                  fun extdW w =
                     if WordSize.equals (ws, workWordSize)
                        then w
                     else Dexp.primApp {prim = Prim.wordExtdToWord 
                                               (ws, workWordSize, 
                                                {signed = false}),
                                        targs = Vector.new0 (),
                                        args = Vector.new1 w,
                                        ty = workTy}

                  val mask = 
                     (Dexp.word o WordX.resize) 
                     (WordX.allOnes WordSize.word8, 
                      workWordSize)

                  fun loop (st, w, b) =
                     if Bits.<= (b, Bits.zero)
                        then st
                     else let
                             val dst0 = st
                             val w0 = Var.newNoname ()
                             val dw0 = Dexp.var (w0, workTy)
                             val bw = Var.newNoname ()
                             val dbw = Dexp.var (bw, workTy)
                             val st1 = Var.newNoname ()
                             val dst1 = Dexp.var (st1, stateTy)
                             val st2 = Var.newNoname ()
                             val dst2 = Dexp.var (st2, stateTy)
                          in
                             Dexp.lett
                             {decs = [{var = w0, exp = w},
                                      {var = bw, exp = 
                                       Dexp.andb (dw0, mask, workWordSize)},
                                      {var = st1, exp = 
                                       combByte (dst0, dbw)},
                                      {var = st2, exp = 
                                       mix dst1}],
                              body = loop (dst2, 
                                           Dexp.rshift (dw0, 
                                                        Dexp.shiftBits Bits.inWord8, 
                                                        workWordSize),
                                           Bits.- (b, Bits.inWord8))}
                          end
                  fun lp (st, w, b) =
                     if Bits.<= (b, Bits.zero)
                        then st
                     else let
                             val dst0 = st
                             val w0 = Var.newNoname ()
                             val dw0 = Dexp.var (w0, Type.word ws)
                             val ew = Var.newNoname ()
                             val dew = Dexp.var (ew, workTy)
                             val loopBits = Bits.min (b, workBits)
                             val st1 = Var.newNoname ()
                             val dst1 = Dexp.var (st1, stateTy)
                          in
                             Dexp.lett
                             {decs = [{var = w0, exp = w},
                                      {var = ew, exp = extdW dw0},
                                      {var = st1, exp = loop (dst0, dew, loopBits)}],
                              body = lp (dst1, 
                                         Dexp.rshift (dw0, 
                                                      Dexp.shiftBits workBits, 
                                                      ws),
                                         Bits.- (b, workBits))}
                          end
                  val st0 = Var.newNoname ()
                  val dst0 = Dexp.var (st0, stateTy)
               in
                  Dexp.lett
                  {decs = [{var = st0, exp = st}],
                   body = lp (dst0, w, WordSize.bits ws)}
               end
         in
            wordBytes
         end

(*
      (* Jenkins hash function
       * http://en.wikipedia.org/wiki/Jenkins_hash_function  (20100315)
       *) 
      val {stateTy: Type.t,
           init: unit -> Dexp.t,
           wordBytes: Dexp.t * Dexp.t * WordSize.t -> Dexp.t,
           fini: Dexp.t -> Dexp.t} =
         let
            val stateWordSize = resWordSize
            val stateTy = Type.word stateWordSize
            val workWordSize = resWordSize
            val workTy = Type.word workWordSize

            local
               fun mk prim =
                  fn (w1, w2) => prim (w1, w2, stateWordSize)
            in
               val add = mk Dexp.add
               val lshift = mk Dexp.lshift
               val rshift = mk Dexp.rshift
               val xorb = mk Dexp.xorb
            end

            fun init () = Dexp.word (WordX.zero stateWordSize)
            fun combByte (hexp, wexp) =
               let
                  val h0 = Var.newNoname ()
                  val dh0 = Dexp.var (h0, stateTy)
                  val w0 = Var.newNoname ()
                  val dw0 = Dexp.var (w0, workTy)
                  val h1 = Var.newNoname ()
                  val dh1 = Dexp.var (h1, stateTy)
               in
                  Dexp.lett
                  {decs = [{var = h0, exp = hexp},
                           {var = w0, exp = wexp},
                           {var = h1, exp = add (dh0, dw0)}],
                   body = dh1}
               end
            fun mix hexp =
               let
                  val h0 = Var.newNoname ()
                  val dh0 = Dexp.var (h0, stateTy)
                  val h1 = Var.newNoname ()
                  val dh1 = Dexp.var (h1, stateTy)
                  val h2 = Var.newNoname ()
                  val dh2 = Dexp.var (h2, stateTy)
               in
                  Dexp.lett
                  {decs = [{var = h0, exp = hexp},
                           {var = h1, exp = add (dh0, lshift (dh0, Dexp.shiftInt 10))},
                           {var = h2, exp = xorb (dh1, rshift (dh1, Dexp.shiftInt 6))}],
                   body = dh2}
               end
            val wordBytes =
               mkWordBytes
               {stateTy = stateTy,
                workWordSize = workWordSize,
                combByte = combByte,
                mix = mix}
            fun fini hexp =
               let
                  val h0 = Var.newNoname ()
                  val dh0 = Dexp.var (h0, stateTy)
                  val h1 = Var.newNoname ()
                  val dh1 = Dexp.var (h1, stateTy)
                  val h2 = Var.newNoname ()
                  val dh2 = Dexp.var (h2, stateTy)
                  val h3 = Var.newNoname ()
                  val dh3 = Dexp.var (h3, stateTy)
               in
                  Dexp.lett
                  {decs = [{var = h0, exp = hexp},
                           {var = h1, exp = add (dh0, lshift (dh0, Dexp.shiftInt 3))},
                           {var = h2, exp = xorb (dh1, rshift (dh1, Dexp.shiftInt 11))},
                           {var = h3, exp = add (dh2, lshift (dh2, Dexp.shiftInt 15))}],
                   body = dh3}
               end
         in
            {stateTy = stateTy,
             init = init,
             wordBytes = wordBytes,
             fini = fini}
         end
*)

      (* FNV-1a hash function
       * http://en.wikipedia.org/wiki/Fowler-Noll-Vo_hash_function  (20100315)
       *)
      val {stateTy: Type.t,
           init: unit -> Dexp.t,
           wordBytes: Dexp.t * Dexp.t * WordSize.t -> Dexp.t,
           fini: Dexp.t -> Dexp.t} =
         let
            val stateWordSize = resWordSize
            val stateTy = Type.word stateWordSize
            val workWordSize = resWordSize
            val workTy = Type.word workWordSize
               
            local
               fun mk prim =
                  fn (w1, w2) => prim (w1, w2, stateWordSize)
            in
               val mul = mk (fn (w1,w2,s) => Dexp.mul (w1,w2,s,{signed = false}))
               val xorb = mk Dexp.xorb
            end

            val fnv_prime = WordX.fromIntInf (16777619, stateWordSize)
            val fnv_offset_bias = WordX.fromIntInf (2166136261, stateWordSize)

            fun init () = Dexp.word fnv_offset_bias
            fun combByte (hexp, wexp) =
               let
                  val h0 = Var.newNoname ()
                  val dh0 = Dexp.var (h0, stateTy)
                  val w0 = Var.newNoname ()
                  val dw0 = Dexp.var (w0, workTy)
                  val h1 = Var.newNoname ()
                  val dh1 = Dexp.var (h1, stateTy)
               in
                  Dexp.lett
                  {decs = [{var = h0, exp = hexp},
                           {var = w0, exp = wexp},
                           {var = h1, exp = xorb (dh0, dw0)}],
                   body = dh1}
               end
            fun mix hexp =
               let
                  val h0 = Var.newNoname ()
                  val dh0 = Dexp.var (h0, stateTy)
                  val p = Dexp.word fnv_prime
                  val h1 = Var.newNoname ()
                  val dh1 = Dexp.var (h1, stateTy)
               in
                  Dexp.lett
                  {decs = [{var = h0, exp = hexp},
                           {var = h1, exp = mul (dh0, p)}],
                   body = dh1}
               end
            val wordBytes =
               mkWordBytes
               {stateTy = stateTy,
                workWordSize = workWordSize,
                combByte = combByte,
                mix = mix}
            fun fini hexp = hexp
         in
            {stateTy = stateTy,
             init = init,
             wordBytes = wordBytes,
             fini = fini}
         end
      fun wordBytesFromWord (st: Dexp.t, w:word, ws: WordSize.t) =
         wordBytes (st, Dexp.wordFromWord (w, ws), ws)
   end

fun transform (Program.T {datatypes, globals, functions, main}) =
   let
      val {get = funcInfo: Func.t -> {hasHash: bool},
           set = setFuncInfo, ...} =
         Property.getSet (Func.plist, Property.initConst {hasHash = false})
      val {get = labelInfo: Label.t -> {hasHash: bool},
           set = setLabelInfo, ...} =
         Property.getSet (Label.plist, Property.initConst {hasHash = false})
      val {get = tyconInfo: Tycon.t -> {cons: {con: Con.t,
                                               args: Type.t vector} vector},
           set = setTyconInfo, ...} =
         Property.getSetOnce
         (Tycon.plist, Property.initRaise ("PolyHash.info", Tycon.layout))
      val tyconCons = #cons o tyconInfo
      val {get = getHashFunc: Type.t -> Func.t option,
           set = setHashFunc,
           destroy = destroyHashFunc} =
         Property.destGetSet (Type.plist, Property.initConst NONE)
      val {get = getTyconHashFunc: Tycon.t -> Func.t option, 
           set = setTyconHashFunc, ...} =
         Property.getSet (Tycon.plist, Property.initConst NONE)
      val {get = getVectorHashFunc: Type.t -> Func.t option, 
           set = setVectorHashFunc, 
           destroy = destroyVectorHashFunc} =
         Property.destGetSet (Type.plist, Property.initConst NONE)
      val returns = SOME (Vector.new1 Hash.stateTy)
      val seqIndexWordSize = WordSize.seqIndex ()
      val seqIndexTy = Type.word seqIndexWordSize
      val newFunctions: Function.t list ref = ref []
      fun newFunction z =
         List.push (newFunctions,
                    Function.profile (Function.new z,
                                      SourceInfo.polyHash))
      fun hashTyconFunc (tycon: Tycon.t): Func.t =
         case getTyconHashFunc tycon of
            SOME f => f
          | NONE =>
               let
                  val name =
                     Func.newString (concat ["hash_", Tycon.originalName tycon])
                  val _ = setTyconHashFunc (tycon, SOME name)
                  val ty = Type.datatypee tycon
                  val st = (Var.newNoname (), Hash.stateTy)
                  val x = (Var.newNoname (), ty)
                  val args = Vector.new2 (st, x)
                  val dst = Dexp.var st
                  val dx = Dexp.var x
                  val cons = tyconCons tycon
                  val body =
                     Dexp.casee
                     {test = dx,
                      ty = Hash.stateTy,
                      default = NONE,
                      cases =
                      (Dexp.Con o Vector.map)
                      (cons, fn {con, args} =>
                       let
                          val xs =
                             Vector.map
                             (args, fn ty =>
                              (Var.newNoname (), ty))
                       in
                          {con = con,
                           args = xs,
                           body =
                           Vector.fold
                           (xs,
                            Hash.wordBytesFromWord
                            (dst, Con.hash con, WordSize.word32),
                            fn ((x,ty), dstate) =>
                            hashExp (dstate, Dexp.var (x, ty), ty))}
                       end)}
                  val (start, blocks) = Dexp.linearize (body, Handler.Caller)
                  val blocks = Vector.fromList blocks
                  val _ =
                     newFunction {args = args,
                                  blocks = blocks,
                                  mayInline = true,
                                  name = name,
                                  raises = NONE,
                                  returns = returns,
                                  start = start}
               in
                  name
               end
      and vectorHashFunc (ty: Type.t): Func.t =
         case getVectorHashFunc ty of
            SOME f => f
          | NONE =>
               let
                  (* Build two functions, one that hashes the length and the
                   * other that loops.
                   *)
                  val name = Func.newString "vectorHash"
                  val _ = setVectorHashFunc (ty, SOME name)
                  val loop = Func.newString "vectorHashLoop"
                  val vty = Type.vector ty
                  local
                     val st = (Var.newNoname (), Hash.stateTy)
                     val vec = (Var.newNoname (), vty)
                     val args = Vector.new2 (st, vec)
                     val dst = Dexp.var st
                     val dvec = Dexp.var vec
                     val len = (Var.newNoname (), seqIndexTy)
                     val dlen = Dexp.var len
                     val body =
                        Dexp.lett
                        {decs = [{var = #1 len, exp = 
                                  Dexp.primApp {prim = Prim.vectorLength,
                                                targs = Vector.new1 ty,
                                                args = Vector.new1 dvec,
                                                ty = seqIndexTy}}],
                         body =
                         Dexp.call
                         {func = loop,
                          args = (Vector.new4
                                  (Hash.wordBytes (dst, dlen, seqIndexWordSize),
                                   dvec, dlen, Dexp.word (WordX.zero seqIndexWordSize))),
                          ty = Hash.stateTy}}
                     val (start, blocks) = Dexp.linearize (body, Handler.Caller)
                     val blocks = Vector.fromList blocks
                  in
                     val _ =
                        newFunction {args = args,
                                     blocks = blocks,
                                     mayInline = true,
                                     name = name,
                                     raises = NONE,
                                     returns = returns,
                                     start = start}
                  end
                  local
                     val st = (Var.newNoname (), Hash.stateTy)
                     val vec = (Var.newNoname (), vty)
                     val len = (Var.newNoname (), seqIndexTy)
                     val i = (Var.newNoname (), seqIndexTy)
                     val args = Vector.new4 (st, vec, len, i)
                     val dst = Dexp.var st
                     val dvec = Dexp.var vec
                     val dlen = Dexp.var len
                     val di = Dexp.var i
                     val body =
                        let
                           val args =
                              Vector.new4
                              (hashExp 
                               (dst, 
                                Dexp.primApp {prim = Prim.vectorSub,
                                              targs = Vector.new1 ty,
                                              args = Vector.new2 (dvec, di),
                                              ty = ty},
                                ty),
                               dvec, 
                               dlen,
                               Dexp.add (di, 
                                         Dexp.word (WordX.one seqIndexWordSize),
                                         seqIndexWordSize))
                        in
                           Dexp.casee
                           {test = Dexp.wordEqual
                                   (di, dlen, seqIndexWordSize),
                            ty = Hash.stateTy,
                            default = NONE,
                            cases = (Dexp.Con o Vector.new2)
                                    ({con = Con.truee,
                                      args = Vector.new0 (),
                                      body = dst},
                                     {con = Con.falsee,
                                      args = Vector.new0 (),
                                      body = Dexp.call {args = args,
                                                        func = loop,
                                                        ty = Hash.stateTy}})}
                        end
                     val (start, blocks) = Dexp.linearize (body, Handler.Caller)
                     val blocks = Vector.fromList blocks
                  in
                     val _ =
                        newFunction {args = args,
                                     blocks = blocks,
                                     mayInline = true,
                                     name = loop,
                                     raises = NONE,
                                     returns = returns,
                                     start = start}
                  end
               in
                  name
               end
      and hashExp (st: Dexp.t, x: Dexp.t, ty: Type.t): Dexp.t =
         Dexp.name (st, fn st =>
         Dexp.name (x, fn x => hash (st, x, ty)))
      and hash (st: Var.t, x: Var.t, ty: Type.t): Dexp.t =
         let
            val dst = Dexp.var (st, Hash.stateTy)
            val dx = Dexp.var (x, ty)
            fun stateful () =
               Hash.wordBytesFromWord
               (dst, Type.hash ty, WordSize.word32)

            val body =
               case Type.dest ty of
                  Type.Array _ => stateful ()
                | Type.CPointer => 
                     let
                        val ws = WordSize.cpointer ()
                        val toWord =
                           Dexp.primApp
                           {prim = Prim.cpointerToWord,
                            targs = Vector.new0 (),
                            args = Vector.new1 dx,
                            ty = Type.word ws}
                     in
                        Hash.wordBytes (dst, toWord, ws)
                     end
                | Type.Datatype tycon =>
                     Dexp.call {func = hashTyconFunc tycon,
                                args = Vector.new2 (dst, dx),
                                ty = Hash.stateTy}
                | Type.IntInf => 
                     let
                        val sws = WordSize.smallIntInfWord ()
                        val bws = WordSize.bigIntInfWord ()
                        val toWord =
                           Dexp.primApp
                           {prim = Prim.intInfToWord,
                            targs = Vector.new0 (),
                            args = Vector.new1 dx,
                            ty = Type.word sws}
                        val toVector =
                           Dexp.primApp
                           {prim = Prim.intInfToVector,
                            targs = Vector.new0 (),
                            args = Vector.new1 dx,
                            ty = Type.vector (Type.word bws)}
                        val w = Var.newNoname ()
                        val dw = Dexp.var (w, Type.word sws)
                        val one = Dexp.word (WordX.one sws)
                     in
                        Dexp.lett
                        {decs = [{var = w, exp = toWord}],
                         body = 
                         Dexp.casee
                         {test = Dexp.wordEqual (Dexp.andb (dw, one, sws), one, sws),
                          ty = Hash.stateTy,
                          default = NONE,
                          cases =
                          (Dexp.Con o Vector.new2)
                          ({con = Con.truee,
                            args = Vector.new0 (),
                            body = Hash.wordBytes (dst, dw, sws)},
                           {con = Con.falsee,
                            args = Vector.new0 (),
                            body = 
                            Dexp.call {func = vectorHashFunc (Type.word bws),
                                       args = Vector.new2 (dst, toVector),
                                       ty = Hash.stateTy}})}}
                     end
                | Type.Real rs =>
                     let
                        val ws = WordSize.fromBits (RealSize.bits rs)
                        val toWord =
                           Dexp.primApp
                           {prim = Prim.realCastToWord (rs, ws),
                            targs = Vector.new0 (),
                            args = Vector.new1 dx,
                            ty = Type.word ws}
                     in
                        Hash.wordBytes (dst, toWord, ws)
                     end
                | Type.Ref _ => stateful ()
                | Type.Thread => stateful ()
                | Type.Tuple tys =>
                     let
                        val max = Vector.length tys - 1
                        (* hash components i, i+1, ... *)
                        fun loop (i: int, dst): Dexp.t =
                           if i > max
                              then dst
                           else let
                                   val ty = Vector.sub (tys, i)
                                   val select =
                                      Dexp.select {tuple = dx,
                                                   offset = i,
                                                   ty = ty}
                                in
                                   loop
                                   (i + 1,
                                    hashExp (dst, select, ty))
                                end
                     in
                        loop (0, dst)
                     end
                | Type.Vector ty =>
                     Dexp.call {func = vectorHashFunc ty,
                                args = Vector.new2 (dst, dx),
                                ty = Hash.stateTy}
                | Type.Weak _ => stateful ()
                | Type.Word ws => Hash.wordBytes (dst, dx, ws)
         in
            body
         end
      fun hashFunc (ty: Type.t): Func.t =
         case getHashFunc ty of
            SOME f => f
          | NONE => 
               let
                  val name = Func.newString "hash"
                  val _ = setHashFunc (ty, SOME name)
                  val x = (Var.newNoname (), ty)
                  val args = Vector.new1 x
                  val sti = Var.newNoname ()
                  val dsti = Dexp.var (sti, Hash.stateTy)
                  val dx = Dexp.var x
                  val stf = Var.newNoname ()
                  val dstf = Dexp.var (stf, Hash.stateTy)
                  val w = Var.newNoname ()
                  val dw = Dexp.var (w, Hash.resTy)
                  val body = 
                     Dexp.lett
                     {decs = [{var = sti, exp = Hash.init ()},
                              {var = stf, exp = hashExp (dsti, dx, ty)},
                              {var = w, exp = Hash.fini dstf}],
                      body = dw}
                  val (start, blocks) = Dexp.linearize (body, Handler.Caller)
                  val blocks = Vector.fromList blocks
                  val _ =
                     newFunction {args = args,
                                  blocks = blocks,
                                  mayInline = true,
                                  name = name,
                                  raises = NONE,
                                  returns = returns,
                                  start = start}
               in
                  name
               end

      val _ =
         Vector.foreach
         (datatypes, fn Datatype.T {tycon, cons} =>
          setTyconInfo (tycon,
                        {cons = cons}))
      val () =
         List.foreach
         (functions, fn f =>
          let
             val {name, blocks, ...} = Function.dest f
          in
             Vector.foreach
             (blocks, fn Block.T {label, statements, ...} =>
              let
                 fun setHasHash () =
                    (setFuncInfo (name, {hasHash = true})
                     ; setLabelInfo (label, {hasHash = true}))
              in
                 Vector.foreach
                 (statements, fn Statement.T {exp, ...} =>
                  (case exp of
                      PrimApp {prim, ...} =>
                         (case Prim.name prim of
                             Prim.Name.MLton_hash => setHasHash ()
                           | _ => ())
                    | _ => ()))
              end)
          end)
      fun doit blocks =
         let
            val blocks = 
               Vector.fold
               (blocks, [], 
                fn (block as Block.T {label, args, statements, transfer}, blocks) =>
                if not (#hasHash (labelInfo label))
                   then block::blocks
                else
                let
                   fun finish ({label, args, statements}, transfer) =
                      Block.T {label = label,
                               args = args,
                               statements = Vector.fromListRev statements,
                               transfer = transfer}
                   val (blocks, las) =
                      Vector.fold
                      (statements, 
                       (blocks, {label = label, args = args, statements = []}),
                       fn (stmt as Statement.T {exp, var, ...}, 
                           (blocks, las as {label, args, statements})) =>
                       let
                         fun normal () = (blocks,
                                          {label = label,
                                           args = args,
                                           statements = stmt::statements})
                       in
                         case exp of
                            PrimApp {prim, targs, args, ...} =>
                               (case (Prim.name prim, Vector.length targs) of
                                   (Prim.Name.MLton_hash, 1) =>
                                      let
                                         val ty = Vector.first targs
                                         val x = Vector.first args
                                         val l = Label.newNoname ()
                                      in
                                        (finish 
                                         (las, 
                                          Call {args = Vector.new1 x,
                                                func = hashFunc ty,
                                                return = Return.NonTail 
                                                         {cont = l,
                                                          handler = Handler.Caller}})
                                         :: blocks,
                                         {label = l,
                                          args = Vector.new1 (valOf var, Hash.resTy),
                                          statements = []})
                                      end
                                 | _ => normal ())
                          | _ => normal ()
                       end)
                in
                   finish (las, transfer)
                   :: blocks
                end)
         in
            Vector.fromList blocks
         end
      val functions =
         List.revMap 
         (functions, fn f =>
          let
             val {args, blocks, mayInline, name, raises, returns, start} =
                Function.dest f
             val f =
                if #hasHash (funcInfo name)
                   then Function.new {args = args,
                                      blocks = doit blocks,
                                      mayInline = mayInline,
                                      name = name,
                                      raises = raises,
                                      returns = returns,
                                      start = start}
                else f
             val () = Function.clear f
          in
             f
          end)
      val program =
         Program.T {datatypes = datatypes,
                    globals = globals,
                    functions = (!newFunctions) @ functions,
                    main = main}
      val _ = destroyHashFunc ()
      val _ = destroyVectorHashFunc ()
      val _ = Program.clearTop program
   in
      program
   end

end
