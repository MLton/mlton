(* ast-to-spec.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
 *)

(*
 * ast-to-spec.sml - Conversion from CKIT "ast" to a "spec" (see spec.sml).
 *
 *  (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure AstToSpec = struct

    structure A = Ast
    structure B = Bindings

    structure SS = StringSet
    structure SM = StringMap

    datatype context = CONTEXT of { gensym: unit -> string, anon: bool }

    exception VoidType
    exception Ellipsis
    exception Duplicate of string
    exception SkipFunction of string

    fun bug m = raise Fail ("AstToSpec: bug: " ^ m)
    fun err m = raise Fail ("AstToSpec: error: " ^ m)
    fun warn m = TextIO.output (TextIO.stdErr, "AstToSpec: warning: " ^ m)

    fun build { bundle, sizes: Sizes.sizes, collect_enums,
                cfiles, match, allSU, eshift, gensym_suffix } =
    let

        val curLoc = ref "?"

        fun warnLoc m = warn (concat [!curLoc, ": ", m])

        val { ast, tidtab, errorCount, warningCount,
              auxiliaryInfo = { aidtab, implicits, env } } = bundle

        fun realFunctionDefComing sy = let
            fun isTheDef (A.DECL (A.FunctionDef (id, _, _), _, _)) =
                Symbol.equal (#name id, sy)
              | isTheDef _ = false
        in
            List.exists isTheDef ast
        end

        val srcOf = SourceMap.locToString

        fun isThisFile SourceMap.UNKNOWN = false
          | isThisFile (SourceMap.LOC { srcFile, ... }) =
            List.exists (fn f => f = srcFile) cfiles orelse
            match srcFile

        fun includedSU (tag, loc) = (allSU orelse isThisFile loc)
        fun includedEnum (tag, loc) = isThisFile loc

        fun includedTy (n, loc) = isThisFile loc

        fun isFunction t = TypeUtil.isFunction tidtab t
        fun getFunction t = TypeUtil.getFunction tidtab t
        fun getCoreType t = TypeUtil.getCoreType tidtab t

        fun constness t =
            if TypeUtil.isConst tidtab t then Spec.RO
            else case getCoreType t of
                     A.Array (_, t) => constness t
                   | _ => Spec.RW

        val sizerec = { sizes = sizes, err = err, warn = warn, bug = bug }

        fun sizeOf t = #bytes (Sizeof.byteSizeOf sizerec tidtab t)

        val bytebits = #bits (#char sizes)
        val intbits = #bits (#int sizes)
        val intalign = #align (#int sizes)

        fun getField (m, l) = Sizeof.getField sizerec (m, l)

        fun fieldOffsets t =
            case Sizeof.fieldOffsets sizerec tidtab t of
                NONE => bug "no field offsets"
              | SOME l => l

        val structs = ref []
        val unions = ref []
        val gtys = ref SM.empty
        val gvars = ref SM.empty
        val gfuns = ref SM.empty
        val named_enums = ref SM.empty
        val anon_enums = ref SM.empty

        val seen_structs = ref SS.empty
        val seen_unions = ref SS.empty

        val nexttag = ref 0
        val tags = Tidtab.uidtab () : (string * bool) Tidtab.uidtab

        fun mk_context_td tdname =
            let val next = ref 0
            in
                CONTEXT
                    { gensym =
                      fn () => let
                             val n = !next
                         in
                             next := n + 1;
                             concat ["'",
                                     if n = 0 then "" else Int.toString n,
                                     tdname]
                         end,
                      anon = false }
            end

        fun mk_context_su (parent_tag, anon) =
            let val next = ref 0
            in
                CONTEXT { gensym =
                          fn () => let
                                 val n = !next
                             in
                                 next := n + 1;
                                 concat [parent_tag, "'", Int.toString n]
                             end,
                          anon = anon }
            end

        val tl_context =
            let val next = ref 0
            in
                CONTEXT { gensym =
                          fn () => let
                                 val n = !next
                             in
                                 next := n + 1;
                                 Int.toString n
                             end,
                          anon = true }
            end

        fun tagname (SOME t, _, _) = (t, false)
          | tagname (NONE, CONTEXT { gensym, anon }, tid) =
            (case Tidtab.find (tags, tid) of
                 SOME ta => ta
               | NONE => let
                     val t = gensym ()
                 in
                     Tidtab.insert (tags, tid, (t, anon));
                     (t, anon)
                 end)

        fun reported_tagname (t, false) = t
          | reported_tagname (t, true) = t ^ gensym_suffix

        fun valty C A.Void = raise VoidType
          | valty C A.Ellipses = raise Ellipsis
          | valty C (A.Qual (q, t)) = valty C t
          | valty C (A.Numeric (_, _, A.SIGNED, A.CHAR, _)) = Spec.BASIC Spec.SCHAR
          | valty C (A.Numeric (_, _, A.UNSIGNED, A.CHAR, _)) = Spec.BASIC Spec.UCHAR
          | valty C (A.Numeric (_, _, A.SIGNED, A.SHORT, _)) = Spec.BASIC Spec.SSHORT
          | valty C (A.Numeric (_, _, A.UNSIGNED, A.SHORT, _)) = Spec.BASIC Spec.USHORT
          | valty C (A.Numeric (_, _, A.SIGNED, A.INT, _)) = Spec.BASIC Spec.SINT
          | valty C (A.Numeric (_, _, A.UNSIGNED, A.INT, _)) = Spec.BASIC Spec.UINT
          | valty C (A.Numeric (_, _, A.SIGNED, A.LONG, _)) = Spec.BASIC Spec.SLONG
          | valty C (A.Numeric (_, _, A.UNSIGNED, A.LONG, _)) = Spec.BASIC Spec.ULONG
          | valty C (A.Numeric (_, _, A.SIGNED, A.LONGLONG, _)) = 
              Spec.BASIC Spec.SLONGLONG
          | valty C (A.Numeric (_, _, A.UNSIGNED, A.LONGLONG, _)) = 
              Spec.BASIC Spec.ULONGLONG
          | valty C (A.Numeric (_, _, _, A.FLOAT, _)) = Spec.BASIC Spec.FLOAT
          | valty C (A.Numeric (_, _, _, A.DOUBLE, _)) = Spec.BASIC Spec.DOUBLE
          | valty C (A.Numeric (_, _, _, A.LONGDOUBLE, _)) = 
              Spec.UNIMPLEMENTED "long double"
          | valty C (A.Array (NONE, t)) = valty C (A.Pointer t)
          | valty C (A.Array (SOME (n, _), t)) =
            let val d = Int.fromLarge n
            in
                if d < 0 then err "negative dimension"
                else Spec.ARR { t = valty C t, d = d, esz = sizeOf t }
            end
          | valty C (A.Pointer t) =
            (case getCoreType t of
                 A.Void => Spec.VOIDPTR
               | A.Function f => fptrty C f
               | _ => Spec.PTR (cobj C t))
          | valty C (A.Function f) = fptrty C f
          | valty C (A.StructRef tid) = typeref (tid, Spec.STRUCT, C)
          | valty C (A.UnionRef tid) = typeref (tid, Spec.UNION, C)
          | valty C (A.EnumRef tid) = typeref (tid, fn t => Spec.ENUM (t, false), C)
          | valty C (A.TypeRef tid) =
            typeref (tid, fn _ => bug "missing typedef info", C)
          | valty C A.Error = err "Error type"

        and valty_nonvoid C t = valty C t
            handle VoidType => err "void variable type"

        and fun_valty_nonvoid C t =
            case valty_nonvoid C t of
                Spec.STRUCT tag =>
                    raise SkipFunction "struct argument not supported"
              | Spec.UNION tag =>
                    raise SkipFunction "union argument not supported"
              | ty => ty

        and typeref (tid, otherwise, C) =
            case Tidtab.find (tidtab, tid) of
                NONE => bug "tid not bound in tidtab"
              | SOME { name = SOME n, ntype = NONE, ... } => otherwise n
              | SOME { name = NONE, ntype = NONE, ... } =>
                bug "both name and ntype missing in tidtab binding"
              | SOME { name, ntype = SOME nct, location, ... } =>
                (case nct of
                     B.Struct (tid, members) =>
                     structty (tid, name, C, members, location)
                   | B.Union (tid, members) =>
                     unionty (tid, name, C, members, location)
                   | B.Enum (tid, edefs) =>
                     enumty (tid, name, C, edefs, location)
                   | B.Typedef (_, t) => let
                         val n =
                             case name of
                                 NONE => bug "missing name in typedef"
                               | SOME n => n
                         val C' = mk_context_td n
                         val res = valty C' t
                         fun sameName { src, name, spec } = name = n
                     in
                         if includedTy (n, location) andalso
                            not (SM.inDomain (!gtys, n)) then
                             gtys := SM.insert (!gtys, n,
                                                { src = srcOf location,
                                                  name = n, spec = res })
                         else ();
                         res
                     end)

        and enumty (tid, name, C, edefs, location) = let
            val (tag_stem, anon) = tagname (name, C, tid)
            val tag = reported_tagname (tag_stem, anon)
            fun one ({ name, uid, location, ctype, kind }, i) =
                { name = Symbol.name name, spec = i }
            val enums = if anon then anon_enums else named_enums
        in
            enums := SM.insert (!enums, tag,
                                { src = srcOf location,
                                  tag = tag,
                                  anon = anon,
                                  descr = tag,
                                  exclude = not (includedEnum (tag, location)),
                                  spec = map one edefs });
            Spec.ENUM (tag, anon)
        end

        and structty (tid, name, C, members, location) = let
            val (tag_stem, anon) = tagname (name, C, tid)
            val tag = reported_tagname (tag_stem, anon)
            val ty = Spec.STRUCT tag
            val C' = mk_context_su (tag_stem, anon)
        in
            if SS.member (!seen_structs, tag) then ()
            else let
                    val _ = seen_structs := SS.add (!seen_structs, tag)

                    val fol = fieldOffsets (A.StructRef tid)
                    val ssize = sizeOf (A.StructRef tid)

                    fun bfspec (offset, bits, shift, (c, t)) = let
                        val offset = offset
                        val bits = Word.fromLargeInt bits
                        val shift = eshift (shift, intbits, bits)
                        val r = { offset = offset,
                                  constness = c,
                                  bits = bits,
                                  shift = shift }
                    in
                        case t of
                            Spec.BASIC Spec.UINT => Spec.UBF r
                          | Spec.BASIC Spec.SINT => Spec.SBF r
                          | _ => err "non-int bitfield"
                    end

                    fun synthetic (synth, (_, false), _) = ([], synth)
                      | synthetic (synth, (endp, true), startp) =
                        if endp = startp then ([], synth)
                        else ([{ name = Int.toString synth,
                                 spec = Spec.OFIELD
                                        { offset = endp,
                                          spec = (Spec.RW,
                                                  Spec.ARR { t = Spec.BASIC Spec.UCHAR,
                                                             d = startp - endp,
                                                             esz = 1 }),
                                          synthetic = true } }],
                              synth+1)

                    fun build ([], synth, gap) =
                        #1 (synthetic (synth, gap, ssize))
                      | build ((t, SOME m, NONE) :: rest, synth, gap) =
                        let val bitoff = #bitOffset (getField (m, fol))
                            val bytoff = bitoff div bytebits
                            val (filler, synth) =
                                synthetic (synth, gap, bytoff)
                            val endp = bytoff + sizeOf t
                        in
                            if bitoff mod bytebits <> 0 then
                                bug "non-bitfield not on byte boundary"
                            else
                                filler @
                                { name = Symbol.name (#name m),
                                  spec = Spec.OFIELD
                                             { offset = bytoff,
                                               spec = cobj C' t,
                                               synthetic = false } } ::
                                build (rest, synth, (endp, false))
                        end
                      | build ((t, SOME m, SOME b) :: rest, synth, gap) =
                        let val bitoff = #bitOffset (getField (m, fol))
                            val bytoff =
                                (intalign * (bitoff div intalign))
                                div bytebits
                            val gap = (#1 gap, true)
                        in
                            { name = Symbol.name (#name m),
                              spec = bfspec (bytoff, b,
                                             bitoff mod intalign,
                                             cobj C' t) } ::
                            build (rest, synth, gap)
                        end
                      | build ((t, NONE, SOME _) :: rest, synth, gap) =
                        build (rest, synth, (#1 gap, true))
                      | build ((_, NONE, NONE) :: _, _, _) =
                        bug "unnamed struct member (not bitfield)"

                    val fields = build (members, 0, (0, false))
                in
                    structs := { src = srcOf location,
                                 tag = tag, 
                                 anon = anon,
                                 size = Word.fromInt ssize,
                                 exclude = not (includedSU (tag, location)),
                                 fields = fields } :: !structs
                end;
            ty
        end

        and unionty (tid, name, C, members, location) = let
            val (tag_stem, anon) = tagname (name, C, tid)
            val tag = reported_tagname (tag_stem, anon)     
            val C' = mk_context_su (tag_stem, anon)
            val ty = Spec.UNION tag
            val lsz = ref 0
            fun mkField (t, m: A.member) = let
                val sz = sizeOf t
            in
                { name = Symbol.name (#name m),
                  spec = Spec.OFIELD { offset = 0,
                                       spec = cobj C' t,
                                       synthetic = false } }
            end
        in
            if SS.member (!seen_unions, tag) then ()
            else let
                    val _ = seen_unions := SS.add (!seen_unions, tag)
                    val all = map mkField members
                in
                    unions := { src = srcOf location,
                                tag = tag,
                                anon = anon,
                                size = Word.fromInt (sizeOf (A.UnionRef tid)),
                                exclude = not (includedSU (tag, location)),
                                all = all } :: !unions
                end;
            ty
        end

        and cobj C t = (constness t, valty_nonvoid C t)

        and fptrty C f = Spec.FPTR (cft C f)

        and cft C (res, args) =
            { res = case getCoreType res of
                        A.Void => NONE
                      | _ => SOME (valty_nonvoid C res),
              args = case args of
                         [(arg, _)] => (case getCoreType arg of
                                       A.Void => []
                                     | _ => [fun_valty_nonvoid C arg])
                       | _ => let fun build [] = []
                                    | build [(x, _)] =
                                      ([fun_valty_nonvoid C x]
                                       handle Ellipsis =>
                                              (warnLoc
                                                   ("varargs not supported; \
                                                    \ignoring the ellipsis\n");
                                                   []))
                                    | build ((x, _) :: xs) =
                                      fun_valty_nonvoid C x :: build xs
                              in
                                  build args
                              end }

        fun ft_argnames (res, args) =
            let val optids = map (fn (_, optid) => optid) args
            in
                if List.exists (not o isSome) optids then NONE
                else SOME (map valOf optids)
            end

        fun functionName (f: A.id, ailo: A.id list option) = let
            val n = Symbol.name (#name f)
            val anlo = Option.map (map (Symbol.name o #name)) ailo
        in
            if n = "_init" orelse n = "_fini" orelse
               SM.inDomain (!gfuns, n) then ()
            else let
                     fun doit () =
                     (case getFunction (#ctype f) of
                          SOME fs =>
                          gfuns := SM.insert (!gfuns, n,
                                              { src = !curLoc,
                                                name = n,
                                                spec = cft tl_context fs,
                                                argnames = anlo })
                        | NONE => bug "function without function type")
                     handle SkipFunction reason =>
                         warnLoc (reason ^ "; skipping function\n")
                 in
                 case #stClass f of
                     A.EXTERN => doit ()
                   | A.DEFAULT => doit ()
                   | A.AUTO => ()
                   | A.REGISTER => ()
                   | A.STATIC => ()
                 end
        end

        fun varDecl (v: A.id) = let
            fun doit () =
                (case getFunction (#ctype v) of
                     SOME fs => if realFunctionDefComing (#name v) then ()
                                else functionName (v, ft_argnames fs)
                   | NONE =>
                     let val n = Symbol.name (#name v)
                     in
                         if SM.inDomain (!gvars, n) then ()
                         else gvars := SM.insert
                                           (!gvars, n,
                                            { src = !curLoc, name = n,
                                              spec = cobj tl_context
                                                          (#ctype v) })
                     end)
        in
            case #stClass v of
                A.EXTERN => doit ()
              | A.DEFAULT => doit ()
              | A.AUTO => ()
              | A.REGISTER => ()
              | A.STATIC => ()
        end

        fun dotid tid =
            (* Spec.SINT is an arbitrary choice; the value gets
             * ignored anyway *)
            (ignore (typeref (tid, fn _ => Spec.BASIC Spec.SINT, tl_context))
             handle VoidType => ())     (* ignore type aliases for void *)

        fun declaration (A.TypeDecl { tid, ... }) = dotid tid
          | declaration (A.VarDecl (v, _)) = varDecl v

        fun coreExternalDecl (A.ExternalDecl d) = declaration d
          | coreExternalDecl (A.FunctionDef (f, argids, _)) =
            functionName (f, SOME argids)
          | coreExternalDecl (A.ExternalDeclExt _) = ()

        fun externalDecl (A.DECL (d, _, l)) =
            if isThisFile l then (curLoc := SourceMap.locToString l;
                                  coreExternalDecl d)
            else ()

        fun doast l = app externalDecl l

        fun gen_enums () = let
            val ael = SM.listItems (!anon_enums)
            val nel = SM.listItems (!named_enums)
            infix $
            fun x $ [] = [x]
              | x $ y = x :: ", " :: y
            fun onev (v as { name, spec }, m) =
                if SM.inDomain (m, name) then raise Duplicate name
                else SM.insert (m, name, v)
            fun onee ({ src, tag, anon, spec, descr, exclude }, (m, sl)) =
                (foldl onev m spec, src $ sl)
        in
            if collect_enums then
                let val (m, sl) = foldl onee (SM.empty, []) ael
                in
                    if SM.isEmpty m then nel
                    else { src = concat (rev sl),
                           tag = "'",
                           anon = false,
                           descr = "collected from unnamed enumerations",
                           exclude = false,
                           spec = SM.listItems m }
                         :: nel
                end handle Duplicate name =>
                           (warn (concat ["constant ", name,
                                          " defined more than once;\
                                          \ disabling `-collect'\n"]);
                            ael @ nel)
            else ael @ nel
        end
    in
        doast ast;
        app (dotid o #1) (Tidtab.listItemsi tidtab);
        { structs = !structs,
          unions = !unions,
          gtys = SM.listItems (!gtys),
          gvars = SM.listItems (!gvars),
          gfuns = SM.listItems (!gfuns),
          enums = gen_enums () } : Spec.spec
    end
end
