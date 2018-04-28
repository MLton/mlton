(* gen.sml
 * 2005 Matthew Fluet (mfluet@acm.org)
 *  Adapted for MLton.
(* Copyright (C) 2005-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)
 *)

(*
 * gen.sml - Generating and pretty-printing ML code implementing a
 *           typed interface to a C program.
 *
 *  (C) 2004  The Fellowship of SML/NJ
 *
 * author: Matthias Blume (blume@tti-c.org)
 *)
structure Gen :
sig
   val gen : {cfiles: string list} -> unit
end =
struct

structure Linkage = Control.Linkage

structure P = PrettyPrint
structure PP = P.PP
structure S = Spec

structure IM = IntMap
structure LIS = LargeIntSet
structure SM = StringMap
structure SS = StringSet

exception Incomplete

val Tuple = P.TUPLE
fun Record [] = P.Unit
  | Record l = P.RECORD l
val Con = P.CON
val Arrow = P.ARROW
val Type = P.Type
val Unit = P.Unit
val ETuple = P.ETUPLE
val EUnit = ETuple []
fun ERecord [] = P.ETUPLE []
  | ERecord l = P.ERECORD l
val EVar = P.EVAR
val EApp = P.EAPP
val EConstr = P.ECONSTR
val ESeq = P.ESEQ
val EPrim = P.EPRIM
val ELet = P.ELET
fun EWord w = EVar ("0wx" ^ Word.toString w)
fun EInt i = EVar (Int.toString i)
fun ELInt i = EVar (LargeInt.toString i)
fun EString s = EVar (concat ["\"", String.toString s, "\""])

fun warn m = Out.output (Out.error, "warning: " ^ m)
fun err m = raise Fail (concat ("gen: " :: m))

fun unimp what = raise Fail ("unimplemented type: " ^ what)
fun unimp_arg what = raise Fail ("unimplemented argument type: " ^ what)
fun unimp_res what = raise Fail ("unimplemented result type: " ^ what)

val writeto = "write'to"

fun gen args =
let
   val {cfiles} = args

   val allSU = !Control.allSU
   val collect_enums = !Control.collect_enums
   val dir = !Control.dir
   val enum_cons = !Control.enum_cons
   val extramembers = !Control.extramembers
   val gensym = !Control.gensym
   val libhandle = !Control.libhandle
   val linkage = !Control.linkage
   val match = !Control.match
   val mlbfile = !Control.mlbfile
   val namedargs = !Control.namedargs
   val prefix = !Control.prefix
   val target = valOf (!Control.target)
   val weight = !Control.weight
   val width = !Control.width

   val gensym_suffix =
      if gensym = "" then "" else "_" ^ gensym
   val {name = targetName,
        sizes = targetSizes,
        endianShift = targetEndianShift} = target
   val targetName = String.toLower targetName
   val {heavy = doheavy, light = dolight} = weight

   val hash_cft = Hash.mkFHasher ()
   val hash_mltype = Hash.mkTHasher ()

   val $? = SM.find
   val %? = IM.find

   local
      val program = "ml-nlffigen"
      val version = "0.9.1"
      val author = "Matthias Blume"
      val email = "blume@tti-c.org"

      val modifications = [{author = "Matthew Fluet",
                            email = "mfluet@acm.org",
                            note = "Adapted for MLton."}]

      val credits =
         concat
         (["(* [by ", author, "'s ",
           program, " (version ", version, ") for ", targetName, "] *)"] @
          (map (fn {author, email, note} =>
                concat ["\n(* [modified by ", author,
                        " (", email, ") <", note, ">] *)"]))
          modifications)
      val commentsto =
         concat ["(* Send comments and suggestions to ", email, ". Thanks! *)"]
      val dontedit = "(* This file has been generated automatically. DO NOT EDIT! *)"
   in
      fun openPP (f, src) =
         let
            val device = CPIFDev.openOut (f, width)
            val stream = PP.openStream device

            fun nl () = PP.newline stream
            fun str s = PP.string stream s
            fun sp () = PP.space stream 1
            fun nsp () = PP.nbSpace stream 1
            fun Box a = PP.openBox stream (PP.Abs a)
            fun HBox () = PP.openHBox stream
            fun HVBox x = PP.openHVBox stream x
            fun HOVBox a = PP.openHOVBox stream (PP.Abs a)
            fun VBox a = PP.openVBox stream (PP.Abs a)
            fun endBox () = PP.closeBox stream
            fun ppty t = P.ppType stream t
            fun ppExp e = P.ppExp stream e
            fun ppFun x = P.ppFun stream x
            fun line s = (nl (); str s)
            fun pr_vdef (v, e) =
               (nl (); HOVBox 4
                ; str "val"; nsp (); str v; nsp (); str "=" ; sp (); ppExp e
                ; endBox ())
            fun pr_fdef (f, args, res) = (nl (); ppFun (f, args, res))
            fun pr_decl (keyword, connector) (v, t) =
               (nl (); HOVBox 4
                ; str keyword; nsp (); str v; nsp (); str connector; sp (); ppty t
                ; endBox ())
            val pr_tdef = pr_decl ("type", "=")
            val pr_vdecl = pr_decl ("val", ":")
            fun closePP () = (PP.closeStream stream; CPIFDev.closeOut device)
         in
            str dontedit;
            case src of
               NONE => ()
             | SOME s =>
                  (nl (); str (concat ["(* [from code at ", s, "] *)"]));
            line credits;
            line commentsto;
            nl ();
            {stream = stream,
             line = line, nl = nl, str = str, sp = sp, nsp = nsp,
             Box = Box, endBox = endBox,
             HVBox = HVBox, HBox = HBox, HOVBox = HOVBox, VBox = VBox,
             ppty = ppty, ppExp = ppExp, ppFun = ppFun,
             pr_vdef = pr_vdef, pr_fdef = pr_fdef,
             pr_tdef = pr_tdef, pr_vdecl = pr_vdecl,
             closePP = closePP}
         end
   end

   local
      val cpp_tmpl =
         Option.fold
         (Process.getEnv "FFIGEN_CPP",
          defaultCppCmd,
          fn (cpp_tmpl,_) => cpp_tmpl)
      val cpp_tmpl =
         String.substituteFirst
         (cpp_tmpl,
          {substring = "%o",
           replacement = String.concatWith (List.rev (!Control.cppopts), " ")})

      fun mkidlsource (cfile,ifile) =
         let
            val cpp =
               List.fold
               ([{substring = "%s", replacement = cfile},
                 {substring = "%t", replacement = ifile}],
                cpp_tmpl,
                fn (s, subst) => String.substituteFirst (subst, s))
         in
            Process.system cpp
         end

      fun getSpec (cfile, s) =
         File.withTemp
         (fn ifile =>
          let
             val () = mkidlsource (cfile, ifile)
             val astbundle =
                ParseToAst.fileToAst'
                Out.error
                (targetSizes, State.INITIAL)
                ifile
             val s' =
                AstToSpec.build
                {bundle = astbundle,
                 sizes = targetSizes,
                 collect_enums = collect_enums,
                 cfiles = cfiles,
                 match = match,
                 allSU = allSU,
                 eshift = targetEndianShift,
                 gensym_suffix = gensym_suffix}
          in
             S.join (s', s)
          end)
   in
      val spec = List.fold (cfiles, S.empty, getSpec)
   end
   val {structs, unions, gvars, gfuns, gtys, enums} = spec

   val (structs, unions, enums) =
      let
         val structs =
            List.fold (structs, SM.empty, fn (s, m) => SM.insert (m, #tag s, s))
         val unions =
            List.fold (unions, SM.empty, fn (s, m) => SM.insert (m, #tag s, s))
         val enums =
            List.fold (enums, SM.empty, fn (s, m) => SM.insert (m, #tag s, s))

         val sdone = ref SS.empty
         val udone = ref SS.empty
         val edone = ref SS.empty
         val smap = ref SM.empty
         val umap = ref SM.empty
         val emap = ref SM.empty
         val ty_queue = ref []
         fun ty_sched ty = List.push (ty_queue, ty)
         fun fs_sched (S.OFIELD { spec = (_, ty), ... }) = ty_sched ty
           | fs_sched _ = ()
         fun f_sched { name, spec } = fs_sched spec

         fun xenter (xdone, xall, xmap, xfields) t =
            if SS.member (!xdone, t) then ()
               else (xdone := SS.add (!xdone, t);
                     case $? (xall, t) of
                        SOME x => (xmap := SM.insert (!xmap, t, x);
                                   app f_sched (xfields x))
                      | NONE => ())

         val senter = xenter (sdone, structs, smap, #fields)
         val uenter = xenter (udone, unions, umap, #all)
         val eenter = xenter (edone, enums, emap, fn _ => [])

         fun sinclude (s: S.s) = if #exclude s then () else senter (#tag s)
         fun uinclude (u: S.u) = if #exclude u then () else uenter (#tag u)
         fun einclude (e: S.enum) = if #exclude e then () else eenter (#tag e)

         fun gty {src, name, spec} = ty_sched spec
         fun gvar {src, name, spec = (_, t)} = ty_sched t
         fun gfun {src, name, spec, argnames} = ty_sched (S.FPTR spec)
         fun loop tys =
            let
               fun do_ty ty =
                  case ty of
                     S.BASIC _ => ()
                   | S.STRUCT t => senter t
                   | S.UNION t => uenter t
                   | S.ENUM (t, anon) =>
                        if collect_enums andalso anon
                           then eenter "'"
                           else eenter t
                   | S.VOIDPTR => ()
                   | S.FPTR {args, res} =>
                        (List.foreach (args, do_ty); Option.app (res, do_ty))
                   | S.PTR (_, S.STRUCT t) => ()
                   | S.PTR (_, S.UNION t) => ()
                   | S.PTR (_, t) => do_ty t
                   | S.ARR {t, ... } => do_ty t
                   | S.UNIMPLEMENTED _ => ()
               fun ty_loop tys =
                  case tys of
                     [] => nextround ()
                   | ty :: tys => (do_ty ty; ty_loop tys)
            in
               case tys of
                  [] => ()
                | _ => (ty_queue := []; ty_loop tys)
            end
         and nextround () = loop (!ty_queue)
      in
         SM.app sinclude structs;
         SM.app uinclude unions;
         SM.app einclude enums;
         app gty gtys;
         app gvar gvars;
         app gfun gfuns;
         nextround ();
         (!smap, !umap, !emap)
      end
   val (fptr_types,incomplete_structs, incomplete_unions, incomplete_enums) =
      let
         fun taginsert (t, ss) =
            if SS.member (ss, t) then ss else SS.add (ss, t)
         fun sinsert (t, (f, s, u, e)) =
            (f, taginsert (t, s), u, e)
         fun uinsert (t, (f, s, u, e)) =
            (f, s, taginsert (t, u), e)
         fun einsert (t, (f, s, u, e)) =
            (f, s, u, taginsert (t, e))
         fun maybe_insert (t, ss, acc, insert) =
            case $? (ss, t) of
               SOME _ => acc
             | NONE => insert (t, acc)

         fun do_ty (ty, acc) =
            case ty of
               S.BASIC _ => acc
             | S.STRUCT t => maybe_insert (t, structs, acc, sinsert)
             | S.UNION t => maybe_insert (t, unions, acc, uinsert)
             | S.ENUM (t, anon) =>
                  if collect_enums andalso anon
                     then acc
                     else maybe_insert (t, enums, acc, einsert)
             | S.VOIDPTR => acc
             | S.FPTR (cft as {args, res}) =>
                  let
                     val acc as (f, s, u, e) =
                        Option.fold (res, List.fold (args, acc, do_ty), do_ty)
                     val cfth = hash_cft cft
                     val i = IM.numItems f
                  in
                     if IM.inDomain (f, cfth)
                        then acc
                        else (IM.insert (f, cfth, (cft, i)), s, u, e)
                  end
             | S.PTR (_, ty) => do_ty (ty, acc)
             | S.ARR {t = ty, ...} => do_ty (ty, acc)
             | S.UNIMPLEMENTED _ => acc

            fun fs (S.OFIELD {spec = (_, ty), ...}, acc) = do_ty (ty, acc)
              | fs (_, acc) = acc
            fun f ({name, spec}, acc) = fs (spec, acc)
            fun s ({src, tag, size, anon, fields, exclude}, acc) =
               List.fold (fields, acc, f)
            fun u ({src, tag, size, anon, all, exclude}, acc) =
               List.fold (all, acc, f)

            fun gvar ({src, name, spec = (_, ty)}, acc) = do_ty (ty, acc)
            fun gfun ({src, name, spec, argnames}, acc) = do_ty (S.FPTR spec, acc)
            fun gty ({src, name, spec}, acc) = do_ty (spec, acc)

            fun lfold (l, f, b) = List.fold (l, b, f)
            fun mfold (m, f, b) = SM.foldl f b m
        in
           lfold (gvars, gvar,
           lfold (gfuns, gfun,
           lfold (gtys, gty,
           mfold (structs, s,
           mfold (unions, u,
           (IM.empty, SS.empty, SS.empty, SS.empty))))))
      end
   fun s_inc t = SS.member (incomplete_structs, t)
   fun u_inc t = SS.member (incomplete_unions, t)

   fun Gstruct n = concat [prefix, "G_", n]
   fun Fstruct n = concat [prefix, "F_", n]
   fun fptr_rtti_struct_id i = "FPtrRTTI_" ^ Int.toString i
   fun Tstruct n = concat [prefix, "T_", n]

   fun SUETstruct K t = concat [K, "T_", t]
   val STstruct = SUETstruct "S"
   val UTstruct = SUETstruct "U"
   fun Suobj'rw p sut = Con ("su_obj" ^ p, [sut, Type "rw"])
   fun Suobj'ro sut = Con ("su_obj'", [sut, Type "ro"])

   fun SUEstruct K t = concat [prefix, K, "_", t]
   val Sstruct = SUEstruct "S"
   val Ustruct = SUEstruct "U"
   val Estruct = SUEstruct "E"
   fun Estruct' (n, anon) =
      Estruct (if anon andalso collect_enums then "'" else n)

   fun fieldtype_id n = "t_f_" ^ n
   fun fieldrtti_id n = "typ_f_" ^ n
   fun field_id (n, p) = concat ["f_", n, p]

   fun enum_id n = "e_" ^ n

   val pending = ref []
   val exports = ref []
   val files = ref []

   local
      val dir_exists = ref false
      val checkDir = fn () =>
         if !dir_exists
            then ()
            else (dir_exists := true;
                  if OS.FileSys.isDir dir handle _ => false
                     then ()
                     else OS.FileSys.mkDir dir)
   in
      fun smlFileAndExport (file,export,do_export) =
         let
            (* We don't want apostrophes in file names -> turn them into minuses.
             * We also want to use only lowercase characters as some file systems
             * are case insensitive.
             *)
            val base = Vector.map (file, fn #"'" => #"-" | c => Char.toLower c)
            fun pick i = let
               val file = OS.Path.joinBaseExt
                             {base = if i=0 then base
                                     else concat [base, "-", Int.toString i],
                              ext = SOME "sml"}
            in
               if List.exists (!files, fn f => f = file) then pick (i+1)
               else file
            end
            val file = pick 0
            val result = OS.Path.joinDirFile {dir = dir, file = file}
         in
            checkDir ()
            ; List.push (pending, export)
            ; (result, fn () => (List.push (files, file)
                                 ; if do_export
                                      then List.push (exports, export)
                                      else ()
                                 ; ignore (List.pop pending)))
         end
      fun descrFile file =
         let
            val result = OS.Path.joinDirFile {dir = dir, file = file}
         in
            checkDir ()
            ; result
         end
   end

   fun rwro_str S.RW = "rw"
     | rwro_str S.RO = "ro"
   fun rwro_type c = Type (rwro_str c)
   fun rwro_c_type S.RW = Type "'c"
     | rwro_c_type S.RO = Type "ro"

   fun dim_ty 0 = Type "dec"
     | dim_ty n = Con ("dg" ^ Int.toString (n mod 10),
                       [dim_ty (n div 10)])
   val dim_ty = fn n =>
      if n < 0
         then raise Fail "negative dimension"
         else dim_ty n

   fun dim_val n =
      let
         fun build 0 = EVar "dec"
           | build n = EApp (build (n div 10),
                             EVar ("dg" ^ Int.toString (n mod 10)))
      in
         EApp (build n, EVar "dim")
      end

   fun stem basic_t =
      case basic_t of
         S.SCHAR => "schar"
       | S.UCHAR => "uchar"
       | S.SSHORT => "sshort"
       | S.USHORT => "ushort"
       | S.SINT => "sint"
       | S.UINT => "uint"
       | S.SLONG => "slong"
       | S.ULONG => "ulong"
       | S.SLONGLONG => "slonglong"
       | S.ULONGLONG => "ulonglong"
       | S.FLOAT => "float"
       | S.DOUBLE => "double"

   val bytebits = #bits (#char targetSizes)
   fun sizeof_basic basic_t =
      case basic_t of
         S.SCHAR => #bits (#char targetSizes)
       | S.UCHAR => #bits (#char targetSizes)
       | S.SSHORT => #bits (#short targetSizes)
       | S.USHORT => #bits (#short targetSizes)
       | S.SINT => #bits (#int targetSizes)
       | S.UINT => #bits (#int targetSizes)
       | S.SLONG => #bits (#long targetSizes)
       | S.ULONG => #bits (#long targetSizes)
       | S.SLONGLONG => #bits (#longlong targetSizes)
       | S.ULONGLONG => #bits (#longlong targetSizes)
       | S.FLOAT => #bits (#float targetSizes)
       | S.DOUBLE => #bits (#double targetSizes)
   and sizeof t =
      case t of
         S.BASIC basic_t => Word.fromInt ((sizeof_basic basic_t) div bytebits)
       | S.STRUCT t =>
            (case $? (structs, t) of
                SOME {size, ...} =>  size
              | NONE => err ["incomplete struct argument: struct ", t])
       | S.UNION t =>
            (case $? (unions, t) of
                SOME {size, ...} => size
              | NONE => err ["incomplete union argument: union ", t])
       | S.ENUM _ => Word.fromInt ((#bits (#int targetSizes)) div bytebits)
       | S.VOIDPTR => Word.fromInt ((#bits (#pointer targetSizes)) div bytebits)
       | S.FPTR _ => Word.fromInt ((#bits (#pointer targetSizes)) div bytebits)
       | S.PTR _ => Word.fromInt ((#bits (#pointer targetSizes)) div bytebits)
       | S.ARR {d, esz, ...} => Word.fromInt (d * esz)
       | S.UNIMPLEMENTED what => unimp what

   val genStructTable : (String.t * unit Promise.t) HashSet.t =
      HashSet.new {hash = fn (structname, _) => String.hash structname}
   fun fillGenStructTable (app, coll, pr_promise) =
      app (coll, fn elem =>
           let val (structname, promise) = pr_promise elem
           in
              (ignore o HashSet.lookupOrInsert)
              (genStructTable, String.hash structname,
               fn (s,_) => String.equals (structname, s),
               fn () => (structname, promise))
           end)
   fun fillGenStructTable' (app, coll, pr_promise) =
      fillGenStructTable (fn (c, f) => app f c, coll, pr_promise)
   fun forceGenStruct structname =
      case HashSet.peek (genStructTable, String.hash structname,
                         fn (s,_) => String.equals (structname, s)) of
         SOME (_,promise) => (Promise.force promise; structname)
       | NONE => err ["missing structure: ", structname]

   fun SUEtag K tag =
      Type ((forceGenStruct (SUETstruct K tag)) ^ ".tag")
   val Stag = SUEtag "S"
   val Utag = SUEtag "U"
   fun Etag (tag, anon) =
      SUEtag "E" (if collect_enums andalso anon then "'" else tag)
   fun SUEtyp K tag =
      EVar ((forceGenStruct (SUETstruct K tag)) ^ ".typ")
   val Styp = SUEtyp "S"
   val Utyp = SUEtyp "U"

   fun fptr_rtti_qid i =
      (forceGenStruct (fptr_rtti_struct_id i)) ^ ".typ"
   fun fptr_mkcall_qid i =
      (forceGenStruct (fptr_rtti_struct_id i)) ^ ".mkcall"

   fun witness_fptr_type_p prime {args, res} =
      let
         fun top_type ty =
            case ty of
               S.STRUCT t => Suobj'ro (Stag t)
             | S.UNION t => Suobj'ro (Utag t)
             | ty => witness_type' ty
         val (res_t, extra_arg_t) =
            case res of
               NONE => (Unit, [])
             | SOME (S.STRUCT t) =>
                  let val ot = Suobj'rw "'" (Stag t)
                  in (ot, [ot])
                  end
             | SOME (S.UNION t) =>
                  let val ot = Suobj'rw "'" (Utag t)
                  in (ot, [ot])
                  end
             | SOME ty => (top_type ty, [])
         val arg_tl = extra_arg_t @ (List.map (args, top_type))
         val dom_t = Tuple arg_tl
         val fct_t = Arrow (dom_t, res_t)
      in
         Con ("fptr" ^ prime, [fct_t])
      end
   and witness_type_p prime ty =
      (case ty of
          S.BASIC basic_t => Type (stem basic_t)
        | S.STRUCT t => Con ("su", [Stag t])
        | S.UNION t => Con ("su", [Utag t])
        | S.ENUM t => Con ("enum", [Etag t])
        | S.VOIDPTR => Type "voidptr"
        | S.FPTR spec => witness_fptr_type_p prime spec
        | S.PTR (c, ty) =>
             Con ("ptr" ^ prime,
                  [Con ("obj", [witness_type ty, rwro_type c])])
        | S.ARR {t = ty, d, ...} =>
             Con ("arr", [witness_type ty, dim_ty d])
        | S.UNIMPLEMENTED what => unimp what)
   and witness_type ty =
      witness_type_p "" ty
   and witness_type' ty =
      witness_type_p "'" ty

   fun topfunc_type prime ({args, res}, argnames) =
      let
         fun top_type ty =
            case ty of
               S.BASIC S.SCHAR => Type "MLRep.Char.Signed.int"
             | S.BASIC S.UCHAR => Type "MLRep.Char.Unsigned.word"
             | S.BASIC S.SSHORT => Type "MLRep.Short.Signed.int"
             | S.BASIC S.USHORT => Type "MLRep.Short.Unsigned.word"
             | S.BASIC S.SINT => Type "MLRep.Int.Signed.int"
             | S.BASIC S.UINT => Type "MLRep.Int.Unsigned.word"
             | S.BASIC S.SLONG => Type "MLRep.Long.Signed.int"
             | S.BASIC S.ULONG => Type "MLRep.Long.Unsigned.word"
             | S.BASIC S.SLONGLONG => Type "MLRep.LongLong.Signed.int"
             | S.BASIC S.ULONGLONG => Type "MLRep.LongLong.Unsigned.word"
             | S.BASIC S.FLOAT => Type "MLRep.Float.real"
             | S.BASIC S.DOUBLE => Type "MLRep.Double.real"
             | S.STRUCT t => Con ("su_obj" ^ prime, [Stag t, Type "'c"])
             | S.UNION t => Con ("su_obj" ^ prime, [Utag t, Type "'c"])
             | S.ENUM _ => Type "MLRep.Int.Signed.int"
             | ty => witness_type_p prime ty
         val (res_t, extra_arg_t, extra_argname) =
            case res of
               NONE => (Unit, [], [])
             | SOME (S.STRUCT t) =>
                  let val ot = Suobj'rw prime (Stag t)
                  in (ot, [ot], [writeto])
                  end
             | SOME (S.UNION t) =>
                  let val ot = Suobj'rw prime (Utag t)
                  in (ot, [ot], [writeto])
                  end
             | SOME ty => (top_type ty, [], [])
         val arg_tl = List.map (args, top_type)
         val arg_t =
            case (namedargs, argnames) of
               (true, SOME nl) =>
                  (Record o List.zip)
                  (extra_argname @ nl,
                   extra_arg_t @ arg_tl)
             | _ => Tuple (extra_arg_t @ arg_tl)
      in
         Arrow (arg_t, res_t)
      end

   fun rtti_type ty =
      Con ("T.typ", [witness_type ty])

   local
      fun simple v = EVar ("T." ^ v)
   in
      fun rtti_val ty =
         case ty of
            S.BASIC basic_t => simple (stem basic_t)
          | S.STRUCT t =>
               if s_inc t then raise Incomplete else Styp t
          | S.UNION t =>
               if u_inc t then raise Incomplete else Utyp t
          | S.ENUM t =>
               EConstr (EVar "T.enum", Con ("T.typ", [Con ("enum", [Etag t])]))
          | S.VOIDPTR => simple "voidptr"
          | S.FPTR cft =>
               let
                  val cfth = hash_cft cft
               in
                  case %? (fptr_types, cfth) of
                     SOME (_, i) => EVar (fptr_rtti_qid i)
                   | NONE => raise Fail "fptr type missing"
               end
          | S.PTR (S.RW, ty) =>
               EApp (EVar "T.pointer", rtti_val ty)
          | S.PTR (S.RO, ty) =>
               EApp (EVar "T.ro", EApp (EVar "T.pointer", rtti_val ty))
          | S.ARR {t = ty, d, ...} =>
               EApp (EVar "T.arr", ETuple [rtti_val ty, dim_val d])
          | S.UNIMPLEMENTED what => raise Incomplete
   end

   fun fptr_mkcall spec =
      let
         val h = hash_cft spec
      in
         case %? (fptr_types, h) of
            SOME (_, i) => fptr_mkcall_qid i
          | NONE => raise Fail "missing fptr_type (mkcall)"
      end

   fun pr_addr_import (pr_fdef, name, attrs) =
       pr_fdef ("h", [EUnit],
                EPrim ("_address \"" ^ name ^ "\" " ^ attrs,
                       Type "CMemory.addr"))

   fun pr_gvar_promise x =
      let
         val {src, name, spec = (c, t)} = x
         val gstruct = Gstruct name
         val gstruct_export = "structure " ^ gstruct
      in
         (gstruct,
          Promise.delay
          (fn () =>
           let
              val (file, done) =
                 smlFileAndExport ("g-" ^ name, gstruct_export, true)
              val {closePP, str, nl, Box, VBox, endBox,
                   pr_fdef, pr_vdef, pr_tdef, ...} =
                 openPP (file, SOME src)
              fun doit () =
                 let
                    val () = pr_tdef ("t", witness_type t)
                    val incomplete =
                       (pr_vdef ("typ",
                                 EConstr (rtti_val t,
                                          Con ("T.typ", [Type "t"])))
                        ; false)
                       handle Incomplee => true
                    val obj' =
                       EConstr (EApp (EVar "mk_obj'", EApp (EVar "h", EUnit)),
                                Con ("obj'", [Type "t", rwro_type c]))
                    val dolight = dolight orelse incomplete
                 in
                    if dolight then pr_fdef ("obj'", [EUnit], obj') else ();
                    if doheavy andalso not incomplete
                       then pr_fdef ("obj", [EUnit],
                                     EApp (EApp (EVar "Heavy.obj", EVar "typ"),
                                           if dolight
                                              then EApp (EVar "obj'", EUnit)
                                              else obj'))
                       else ()
                 end
           in
              str "local";
              VBox 4;
              nl (); str "open C.Dim C_Int";
              case linkage of
                 Control.Linkage.Archive =>
                 pr_addr_import (pr_fdef, name, "public")
               | Control.Linkage.Dynamic =>
                    pr_vdef ("h", EApp (EVar libhandle, EString name))
               | Control.Linkage.Shared =>
                 pr_addr_import (pr_fdef, name, "external");
              endBox ();
              nl (); str "in";
              VBox 4;
              nl (); str (gstruct_export ^ " = struct");
              Box 4;
              doit ();
              endBox ();
              nl (); str "end";
              endBox ();
              nl (); str "end"; nl ();
              closePP ();
              done ()
           end))
      end
   val () = fillGenStructTable (List.foreach, gvars, pr_gvar_promise)

   fun pr_gfun_promise x =
      let
         val {src, name, spec as {args, res}, argnames} = x
         val fstruct = Fstruct name
         val fstruct_export = "structure " ^ fstruct
      in
         (fstruct,
          Promise.delay
          (fn () =>
           let
              val (file, done) =
                 smlFileAndExport ("f-" ^ name, fstruct_export, true)
              val {closePP, str, nl, Box, VBox, endBox,
                   pr_fdef, pr_vdef, pr_vdecl, ...} =
                 openPP (file, SOME src)
              fun doit is_light =
                 let
                    val ml_vars =
                       List.mapi
                       (args, fn (i, _) =>
                        EVar ("x" ^ Int.toString (i + 1)))
                    fun app0 (what, e) =
                       if is_light then e else EApp (EVar what, e)
                    fun light (what, e) = app0 ("Light." ^ what, e)
                    fun heavy (what, t, e) =
                       if is_light
                          then e
                          else EApp (EApp (EVar ("Heavy." ^ what), rtti_val t), e)
                    fun oneArg (e, t) =
                       case t of
                          S.BASIC basic_t =>
                             EApp (EVar ("Cvt.c_" ^ stem basic_t), e)
                        | S.STRUCT _ => EApp (EVar "ro'", light ("obj", e))
                        | S.UNION _ => EApp (EVar "ro'", light ("obj", e))
                        | S.ENUM _ => EApp (EVar "Cvt.i2c_enum", e)
                        | S.PTR _ => light ("ptr", e)
                        | S.FPTR _ => light ("fptr", e)
                        | S.VOIDPTR => e
                        | S.UNIMPLEMENTED what => unimp_arg what
                        | S.ARR _ => raise Fail "array argument type"
                    val c_exps = List.map2 (ml_vars, args, oneArg)
                    val (ml_vars, c_exps, extra_argname) =
                       let
                          fun do_su () =
                             let val x0 = EVar "x0"
                             in
                                (x0 :: ml_vars,
                                 light ("obj", x0) :: c_exps,
                                 [writeto])
                             end
                       in
                          case res of
                             SOME (S.STRUCT _) => do_su ()
                           | SOME (S.UNION _) => do_su ()
                           | _ => (ml_vars, c_exps, [])
                       end
                    val call = EApp (EVar "call",
                                     ETuple [EApp (EVar "fptr", EUnit),
                                             ETuple c_exps])
                    val ml_res =
                       case res of
                          NONE => call
                        | SOME t =>
                             (case t of
                                 S.BASIC basic_t =>
                                    EApp (EVar ("Cvt.ml_" ^ stem basic_t), call)
                               | S.STRUCT _ => heavy ("obj", t, call)
                               | S.UNION _ => heavy ("obj", t, call)
                               | S.ENUM _ => EApp (EVar "Cvt.c2i_enum", call)
                               | S.PTR _ => heavy ("ptr", t, call)
                               | S.FPTR _ => heavy ("fptr", t, call)
                               | S.VOIDPTR => call
                               | S.UNIMPLEMENTED what => unimp_res what
                               | S.ARR _ => raise Fail "array result type")
                 in
                    fn () =>
                    pr_fdef (if is_light then "f'" else "f", [ETuple ml_vars], ml_res)
                 end
              fun do_fsig is_light =
                 let val prime = if is_light then "'" else ""
                 in pr_vdecl ("f" ^ prime, topfunc_type prime (spec, argnames))
                 end
              val (do_f_heavy, incomplete) =
                 (if doheavy then doit false else (fn () => ()), false)
                 handle Incomplete => (fn () => (), true)
              val do_f_light =
                 if dolight orelse incomplete then doit true else (fn () => ())
           in
              str "local";
              VBox 4;
              nl (); str "open C.Dim C_Int";
              case linkage of
                 Control.Linkage.Archive =>
                 pr_addr_import (pr_fdef, name, "public")
               | Control.Linkage.Dynamic =>
                    pr_vdef ("h", EApp (EVar libhandle, EString name))
               | Control.Linkage.Shared =>
                 pr_addr_import (pr_fdef, name, "external");
              endBox ();
              nl (); str "in";
              VBox 4;
              nl (); str (fstruct_export ^ " : sig");
              Box 4;
              pr_vdecl ("typ", rtti_type (S.FPTR spec));
              pr_vdecl ("fptr", Arrow (Unit, witness_type (S.FPTR spec)));
              if doheavy andalso not incomplete then do_fsig false else ();
              if dolight orelse incomplete then do_fsig true else ();
              endBox ();
              nl (); str "end = struct";
              Box 4;
              pr_vdef ("typ", rtti_val (S.FPTR spec));
              pr_fdef ("fptr",
                       [EUnit],
                       EApp (EVar "mk_fptr",
                             ETuple [EVar (fptr_mkcall spec),
                                     EApp (EVar "h", EUnit)]));
              do_f_heavy ();
              do_f_light ();
              endBox ();
              nl (); str "end";
              endBox ();
              nl (); str "end"; nl ();
              closePP ();
              done ()
           end))
      end
   val () = fillGenStructTable (List.foreach, gfuns, pr_gfun_promise)

   val get_callop =
      let
         val ncallops = ref 0
         val callops = ref IM.empty
         fun callop_sid i = "Callop_" ^ Int.toString i
         fun callop_qid i = callop_sid i ^ ".callop"
         fun get (ml_args_t, ml_res_t) =
            let
               val e_proto_hash = hash_mltype (Arrow (ml_args_t, ml_res_t))
            in
               case %? (!callops, e_proto_hash) of
                  SOME i => callop_qid i
                | NONE =>
                     let
                        val i = !ncallops
                        val sn = callop_sid i
                        val sn_export = "structure " ^ sn
                        val (file, done) =
                           smlFileAndExport
                           ("callop-" ^ Int.toString i, sn_export, false)
                        val {closePP, str, nl, Box, VBox, endBox,
                             pr_fdef, pr_vdef, pr_tdef, ...} =
                           openPP (file, NONE)
                     in
                        ncallops := i + 1;
                        callops := IM.insert (!callops, e_proto_hash, i);
                        str (sn_export ^ " = struct");
                        Box 4;
                        pr_vdef ("callop",
                                 EPrim ("_import *",
                                        Arrow (Type "CMemory.addr",
                                               Arrow (ml_args_t,
                                                      ml_res_t))));
                        endBox ();
                        nl (); str "end"; nl ();
                        closePP ();
                        done ();
                        callop_qid i
                     end
            end
      in
         get
      end

   fun pr_fptr_rtti_promise x =
      let
         val ({args, res}, i) = x
         val fstruct = fptr_rtti_struct_id i
         val fstruct_export = "structure " ^ fstruct
      in
         (fstruct,
          Promise.delay
          (fn () =>
           let
              val (file, done) =
                 smlFileAndExport ("fptr-rtti-" ^ (Int.toString i), fstruct_export, false)
              val {closePP, str, nl, Box, VBox, endBox,
                   pr_fdef, pr_vdef, pr_tdef, ...} =
                 openPP (file, NONE)

              fun mlty ty =
                 case ty of
                    S.BASIC basic_t => Type ("CMemory.cc_" ^ stem basic_t)
                  | S.STRUCT _ => Type "CMemory.cc_addr"
                  | S.UNION _ => Type "CMemory.cc_addr"
                  | S.ENUM _ => Type "CMemory.cc_sint"
                  | S.VOIDPTR => Type "CMemory.cc_addr"
                  | S.FPTR _ => Type "CMemory.cc_addr"
                  | S.PTR _ => Type "CMemory.cc_addr"
                  | S.ARR _ => raise Fail "unexpected type"
                  | S.UNIMPLEMENTED what => unimp what
              fun wrap (e, n) =
                 EApp (EVar ("CMemory.wrap_" ^ n),
                       EApp (EVar ("Cvt.ml_" ^ n), e))
              fun fldwrap (e, n, alt) =
                 EApp (EVar ("CMemory.wrap_" ^ n),
                       EApp (EVar ("Get." ^ n ^ alt), e))
              fun vwrap e =
                 EApp (EVar "CMemory.wrap_addr",
                       EApp (EVar "reveal", e))
              fun fwrap e =
                 EApp (EVar "CMemory.wrap_addr",
                       EApp (EVar "freveal", e))
              fun pwrap e =
                 EApp (EVar "CMemory.wrap_addr",
                       EApp (EVar "reveal",
                             EApp (EVar "Ptr.inject'", e)))
              fun fldvwrap (e, alt) =
                 EApp (EVar "CMemory.wrap_addr",
                       EApp (EVar "reveal",
                             EApp (EVar ("Get.voidptr" ^ alt), e)))
              fun fldfwrap (e, alt) =
                 EApp (EVar "CMemory.wrap_addr",
                       EApp (EVar "freveal",
                             if alt = "'"
                                then EApp (EVar "Get.fptr'", e)
                                else EApp (EVar "Light.fptr",
                                           EApp (EVar "Get.fptr", e))))
              fun fldpwrap (e, alt) =
                 EApp (EVar "CMemory.wrap_addr",
                       EApp (EVar "reveal",
                             EApp (EVar ("Ptr.inject" ^ alt),
                                   EApp (EVar ("Get.ptr" ^ alt), e))))
              fun suwrap e =
                 pwrap (EApp (EVar "Ptr.|&!", e))
              fun ewrap e =
                 EApp (EVar "CMemory.wrap_sint",
                       EApp (EVar "Cvt.c2i_enum", e))
              fun fldewrap (e, alt) =
                 EApp (EVar "CMemory.wrap_sint",
                       EApp (EVar ("Get.enum" ^ alt), e))

              val (ml_res_t,
                   extra_arg_v, extra_arg_e, extra_ml_arg_t,
                   res_wrap) =
                 case res of
                    NONE =>
                       (Unit, [], [], [], fn r => r)
                  | SOME (S.STRUCT _) =>
                       (Unit,
                        [EVar "x0"],
                        [suwrap (EVar "x0")],
                        [Type "CMemory.cc_addr"],
                        fn r => ESeq (r, EVar "x0"))
                  | SOME (S.UNION _) =>
                       (Unit,
                        [EVar "x0"],
                        [suwrap (EVar "x0")],
                        [Type "CMemory.cc_addr"],
                        fn r => ESeq (r, EVar "x0"))
                  | SOME t =>
                       let
                          fun unwrap n r =
                             EApp (EVar ("Cvt.c_" ^ n),
                                   EApp (EVar ("CMemory.unwrap_" ^ n), r))
                          fun punwrap cast r =
                             EApp (EVar cast,
                                   EApp (EVar "CMemory.unwrap_addr", r))
                          fun eunwrap r =
                             EApp (EVar "Cvt.i2c_enum",
                                   EApp (EVar "CMemory.unwrap_sint", r))
                          val res_wrap =
                             case t of
                                S.BASIC basic_t => unwrap (stem basic_t)
                              | S.STRUCT _ =>
                                   raise Fail "unexpected result type"
                              | S.UNION _ =>
                                   raise Fail "unexpected result type"
                              | S.ENUM _ => eunwrap
                              | S.VOIDPTR => punwrap "vcast"
                              | S.FPTR _ => punwrap "fcast"
                              | S.PTR _ => punwrap "pcast"
                              | S.ARR _ =>
                                   raise Fail "unexpected result type"
                              | S.UNIMPLEMENTED what => unimp_res what
                       in
                          (mlty t, [], [], [], res_wrap)
                       end

              fun doarg (h, p) =
                 let
                    fun sel e = ([mlty h], [e], [])
                 in
                    case h of
                       S.BASIC basic_t => sel (wrap (p, stem basic_t))
                     | S.STRUCT t => (* sel (suwrap p) *)
                          raise Fail "struct argument not (yet) supported"
                     | S.UNION t => (* sel (suwrap p) *)
                          raise Fail "union argument not (yet) supported"
                     | S.ENUM _ => sel (ewrap p)
                     | S.VOIDPTR => sel (vwrap p)
                     | S.FPTR _ => sel (fwrap p)
                     | S.PTR _ => sel (pwrap p)
                     | S.ARR _ => raise Fail "unexpected array argument"
                     | S.UNIMPLEMENTED what => unimp_arg what
                 end
              and arglist ([], _) = ([], [], [])
                | arglist (h :: tl, i) =
                 let
                    val p = EVar ("x" ^ Int.toString i)
                    val (ta, ea, bnds) = arglist (tl, i + 1)
                    val (ta', ea', bnds') = doarg (h, p)
                 in
                    (ta' @ ta, ea' @ ea, bnds' @ bnds)
                 end

              val (ml_args_tl, args_el, bnds) = arglist (args, 1)

              val ml_args_t = Tuple (extra_ml_arg_t @ ml_args_tl)

              val arg_vl =
                 List.mapi
                 (args, fn (i, _) =>
                  EVar ("x" ^ Int.toString (i + 1)))

              val arg_e = ETuple (extra_arg_e @ args_el)
              val callop_n = get_callop (ml_args_t, ml_res_t)
           in
              str "local";
              VBox 4;
              nl (); str "open C.Dim C_Int";
              endBox ();
              nl (); str "in";
              VBox 4;
              nl (); str (fstruct_export ^ " = struct");
              Box 4;
              pr_fdef ("mkcall",
                       [EVar "a", ETuple (extra_arg_v @ arg_vl)],
                       res_wrap (ELet (bnds,
                                       EApp (EApp (EVar callop_n,
                                                   EVar "a"),
                                             arg_e))));
              pr_vdef ("typ",
                       EConstr (EApp (EVar "mk_fptr_typ", EVar "mkcall"),
                                rtti_type (S.FPTR {args = args, res = res})));
              endBox ();
              nl (); str "end";
              endBox ();
              nl (); str "end"; nl ();
              closePP ();
              done ()
           end))
      end
   val () = fillGenStructTable' (IM.app, fptr_types, pr_fptr_rtti_promise)

   fun pr_gty_promise x =
      let
         val {src, name, spec} = x
         val tstruct = Tstruct name
         val tstruct_export = "structure " ^ tstruct
      in
         (tstruct,
          Promise.delay
          (fn () =>
           let
              val (file, done) =
                 smlFileAndExport ("t-" ^ name, tstruct_export, true)
              val {closePP, str, nl, Box, VBox, endBox,
                   pr_vdef, pr_tdef, ...} =
                 openPP (file, SOME src)
              val rtti_val_opt =
                 (SOME (rtti_val spec))
                 handle Incomplete => NONE
           in
              str "local";
              VBox 4;
              nl (); str "open C.Dim C_Int";
              endBox ();
              nl (); str "in";
              VBox 4;
              nl (); str (tstruct_export ^ " = struct");
              Box 4;
              pr_tdef ("t", witness_type spec);
              Option.app
              (rtti_val_opt, fn rtti_val =>
               pr_vdef ("typ", EConstr (rtti_val, Con ("T.typ", [Type "t"]))));
              endBox ();
              nl (); str "end";
              endBox ();
              nl (); str "end"; nl ();
              closePP ();
              done ()
           end))
      end
   val () = fillGenStructTable (List.foreach, gtys, pr_gty_promise)

   datatype sue_szinfo =
      T_INC         (* generate no RTTI *)
    | T_SU of word  (* generate struct/union RTTI *)
    | T_E           (* generate enum RTTI *)

   fun pr_suet_promise x =
      let
         val (src, tag, anon, tinfo, k, K) = x
         val suetstruct = SUETstruct K tag
         val suetstruct_export = "structure " ^ suetstruct
      in
         (suetstruct,
          Promise.delay
          (fn () =>
           let
              val (file, done) =
                 smlFileAndExport (k ^ "t-" ^ tag, suetstruct_export, tinfo = T_INC)
              val {closePP, str, nl, Box, VBox, endBox,
                   pr_vdef, pr_tdef, ...} =
                 openPP (file, src)
              val (utildef, tag_t) =
                 if anon
                    then
                       ("structure X :> sig type t end \
                        \= struct type t = unit end",
                        Type "X.t")
                    else
                       ("open Tag",
                        Vector.foldr
                        (tag, Type k, fn (c, tag_t) =>
                         Con ("t_" ^ String.fromChar c, [tag_t])))
              fun do_susize size =
                 let in
                    pr_vdef ("size",
                             EConstr (EApp (EVar "mk_su_size", EWord size),
                                      Con ("S.size", [Con ("su", [Type "tag"])])));
                    pr_vdef ("typ",
                             EApp (EVar "mk_su_typ", EVar "size"))
                 end
           in
              str "local";
              VBox 4;
              nl (); str "open C.Dim C_Int";
              nl (); str (concat ["structure ", SUEstruct K tag, " = struct"]);
              Box 4;
              nl (); str "local";
              VBox 4;
              nl (); str utildef;
              endBox ();
              nl (); str "in";
              VBox 4;
              pr_tdef ("tag", tag_t);
              endBox ();
              nl (); str "end";
              case tinfo of
                 T_INC => ()
               | T_SU size => do_susize size
               | T_E => ();
              endBox ();
              nl (); str "end";
              endBox ();
              nl (); str "in";
              VBox 4;
              nl (); str (concat [suetstruct_export, " = ", SUEstruct K tag]);
              endBox ();
              nl (); str "end"; nl ();
              closePP ();
              done ()
           end))
      end
   local
      fun pr_st_promise {src, tag, anon, size, fields, exclude} =
         pr_suet_promise (SOME src, tag, anon, T_SU size, "s", "S")
      fun pr_ut_promise {src, tag, anon, size, all, exclude} =
         pr_suet_promise (SOME src, tag, anon, T_SU size, "u", "U")
      fun pr_et_promise {src, tag, anon, descr, spec, exclude} =
         pr_suet_promise (SOME src, tag, anon, T_E, "e", "E")
   in
      val () = fillGenStructTable' (SM.app, structs, pr_st_promise)
      val () = fillGenStructTable' (SM.app, unions, pr_ut_promise)
      val () = fillGenStructTable' (SM.app, enums, pr_et_promise)
   end
   local
      fun pr_i_suet_promise (tag, k, K) =
         pr_suet_promise (NONE, tag, false, T_INC, k, K)
      fun pr_i_st_promise tag = pr_i_suet_promise (tag, "s", "S")
      fun pr_i_ut_promise tag = pr_i_suet_promise (tag, "u", "U")
      fun pr_i_et_promise tag = pr_i_suet_promise (tag, "e", "E")
   in
      val () = fillGenStructTable' (SS.app, incomplete_structs, pr_i_st_promise)
      val () = fillGenStructTable' (SS.app, incomplete_unions, pr_i_ut_promise)
      val () = fillGenStructTable' (SS.app, incomplete_enums, pr_i_et_promise)
   end

   fun pr_su_promise x =
      let
         val (src, tag, fields, k, K) = x
         val sustruct = SUEstruct K tag
         val sustruct_export = "structure " ^ sustruct
      in
         (sustruct,
          Promise.delay
          (fn () =>
           let
              val (file, done) =
                 smlFileAndExport (k ^ "-" ^ tag, sustruct_export, true)
              val {closePP, str, nl, Box, VBox, endBox,
                   pr_fdef, pr_vdef, pr_tdef, ...} =
                 openPP (file, SOME src)
              fun pr_field_type {name, spec} =
                 case spec of
                    S.OFIELD {spec = (c, ty), synthetic = false, offset} =>
                       pr_tdef (fieldtype_id name,
                                witness_type ty)
                  | _ => ()
              fun pr_field_rtti {name, spec} =
                 case spec of
                    S.OFIELD {spec = (c, ty), synthetic = false, offset} =>
                       pr_vdef (fieldrtti_id name,
                                EConstr (rtti_val ty,
                                         Con ("T.typ", [Type (fieldtype_id name)])))
                  | _ => ()
              fun arg_x prime =
                 EConstr (EVar "x",
                          Con ("su_obj" ^ prime,
                               [Type "tag", Type "'c"]))
              fun pr_bf_acc (name, prime, sign,
                             {offset, constness, bits, shift}) =
                 let
                    val maker =
                       concat ["mk_", rwro_str constness, "_", sign, "bf", prime]
                 in
                    pr_fdef (field_id (name, prime),
                             [arg_x prime],
                             EApp (EApp (EVar maker,
                                         ETuple [EInt offset,
                                                 EWord bits,
                                                 EWord shift]),
                                   EVar "x"))
                 end
              fun pr_field_acc' {name, spec} =
                 case spec of
                    S.OFIELD {spec = (c, ty), synthetic, offset} =>
                       if synthetic
                          then ()
                          else pr_fdef (field_id (name, "'"),
                                        [arg_x "'"],
                                        EConstr (EApp (EVar "mk_field'",
                                                       ETuple [EInt offset,
                                                               EVar "x"]),
                                                 Con ("obj'",
                                                      [Type (fieldtype_id name),
                                                       rwro_c_type c])))
                  | S.SBF bf =>
                       pr_bf_acc (name, "'", "s", bf)
                  | S.UBF bf =>
                       pr_bf_acc (name, "'", "u", bf)
              fun pr_field_acc {name, spec} =
                 case spec of
                    S.OFIELD {spec = (c, ty), synthetic, offset} =>
                       if synthetic
                          then ()
                          else let
                                  val maker =
                                     concat ["mk_", rwro_str c, "_field"]
                               in
                                  pr_fdef (field_id (name, ""),
                                           [arg_x ""],
                                           EApp (EVar maker,
                                                 ETuple [EVar (fieldrtti_id name),
                                                         EInt offset,
                                                         EVar "x"]))
                               end
                  | S.SBF bf =>
                       pr_bf_acc (name, "", "s", bf)
                  | S.UBF bf =>
                       pr_bf_acc (name, "", "u", bf)
              fun pr_one_field f =
                 let
                    val _ = pr_field_type f
                    val incomplete =
                       (pr_field_rtti f; false)
                       handle Incomplete => true
                 in
                    if dolight orelse incomplete then pr_field_acc' f else ();
                    if doheavy andalso not incomplete then pr_field_acc f else ()
                 end
           in
              str "local";
              VBox 4;
              nl (); str "open C.Dim C_Int";
              endBox ();
              nl (); str "in";
              VBox 4;
              nl (); str (sustruct_export ^ " = struct");
              Box 4;
              nl (); str ("open " ^ (forceGenStruct (SUETstruct K tag)));
              List.foreach (fields, pr_one_field);
              endBox ();
              nl (); str "end";
              endBox ();
              nl (); str "end"; nl ();
              closePP ();
              done ()
           end))
      end
   local
      fun pr_s_promise { src, tag, anon, size, fields, exclude } =
         pr_su_promise (src, tag, fields, "s", "S")
      fun pr_u_promise { src, tag, anon, size, all, exclude } =
         pr_su_promise (src, tag, all, "u", "U")
   in
      val () = fillGenStructTable' (SM.app, structs, pr_s_promise)
      val () = fillGenStructTable' (SM.app, unions, pr_u_promise)
   end

   fun pr_e_promise x =
      let
         val {src, tag, anon, descr, spec, exclude} = x
         val estruct = Estruct' (tag, anon)
         val estruct_export = "structure " ^ estruct
      in
         (estruct,
          Promise.delay
          (fn () =>
           let
              val (file, done) =
                 smlFileAndExport ("e-" ^ tag, estruct_export, true)
              val {closePP, str, line, nl, sp, Box, VBox, endBox,
                   pr_fdef, pr_vdef, pr_tdef, ...} =
                 openPP (file, SOME src)
              fun no_duplicate_values () =
                 let
                    fun loop (l, s) =
                       case l of
                          [] => true
                        | {name, spec} :: l =>
                             if LIS.member (s, spec)
                                then (warn (concat ["enum ", descr,
                                                    " has duplicate values;\
                                                    \ using sing,\
                                                    \ not generating constructors\n"]);
                                      false)
                                else loop (l, LIS.add (s, spec))
                 in
                    loop (spec, LIS.empty)
                 end
              val dodt = enum_cons andalso no_duplicate_values ()
              fun dt_mlrep () =
                 let
                    fun pcl () =
                       let
                          fun loop (c, l) =
                             case l of
                                [] => ()
                              | {name, spec} :: l =>
                                   (str (c ^ enum_id name); nextround l)
                          and nextround [] = ()
                            | nextround l = (sp (); loop ("| ", l))
                       in
                          Box 2; nl ();
                          loop ("  ", spec);
                          endBox ()
                       end
                    fun pfl (fname, arg, res, fini: unit -> unit) =
                       let
                          fun loop (pfx, l) =
                             case l of
                                [] => ()
                              | v :: l =>
                                   (line (concat [pfx, " ", arg v, " => ", res v]);
                                      loop ("  |", l))
                       in
                          line (concat ["fun ", fname, " x ="]);
                          Box 4;
                          line ("case x of");
                          loop ("   ", spec);
                          fini ();
                          endBox ()
                       end
                    fun cstr {name, spec} = enum_id name
                    fun vstr {name, spec} =
                       LargeInt.toString spec ^ " : MLRep.Int.Signed.int"
                 in
                    line "datatype mlrep =";
                    pcl ();
                    pfl ("m2i", cstr, vstr, fn () => ());
                    pfl ("i2m", vstr, cstr,
                         fn () => line "  | _ => raise General.Domain")
                 end
              fun int_mlrep () =
                 let
                    fun v {name, spec} =
                       pr_vdef (enum_id name, EConstr (ELInt spec, Type "mlrep"))
                    val mlx = EConstr (EVar "x", Type "mlrep")
                    val ty = Type "MLRep.Int.Signed.int"
                    val ix = EConstr (EVar "x", ty)
                 in
                    pr_tdef ("mlrep", ty);
                    List.foreach (spec, v);
                    pr_fdef ("m2i", [mlx], ix);
                    pr_fdef ("i2m", [ix], mlx)
                 end
              fun getset p =
                 let
                    fun constr c = Con ("enum_obj" ^ p, [Type "tag", Type c])
                 in
                    pr_fdef ("get" ^ p,
                             [EConstr (EVar "x", constr "'c")],
                             EApp (EVar "i2m",
                                   EApp (EVar ("Get.enum" ^ p), EVar "x")));
                    pr_fdef ("set" ^ p,
                             [ETuple [EConstr (EVar "x", constr "rw"), EVar "v"]],
                             EApp (EVar ("Set.enum" ^ p),
                                   ETuple [EVar "x", EApp (EVar "m2i", EVar "v")]))
                 end
           in
              str "local";
              VBox 4;
              nl (); str "open C.Dim C_Int";
              endBox ();
              nl (); str "in";
              VBox 4;
              nl (); str (estruct_export ^ " = struct");
              Box 4;
              nl (); str ("open " ^ (forceGenStruct (SUETstruct "E" tag)));
              if dodt then dt_mlrep () else int_mlrep ();

              endBox ();
              nl (); str "end";
              endBox ();
              nl (); str "end"; nl ();
              closePP ();
              done ()
           end))
      end
   val () = fillGenStructTable' (SM.app, enums, pr_e_promise)

   fun do_mlbfile () =
      let
         val file = descrFile mlbfile
         val () = File.remove file
         val {closePP, line, str, nl, VBox, endBox, ... } =
            openPP (file, NONE)
      in
         line "local ann \"allowFFI true\" in";
         VBox 4;
         app line ["$(SML_LIB)/basis/basis.mlb",
                   "$(SML_LIB)/mlnlffi-lib/internals/c-int.mlb"];
         app line (rev extramembers);
         app line (rev (!files));
         endBox ();
         nl (); str "end in";
         VBox 4;
         app line (rev (!exports));
         endBox ();
         nl (); str "end"; nl ();
         closePP ()
      end
in
   (HashSet.foreach (genStructTable, fn (_, promise) => Promise.force promise)
    ; do_mlbfile ())
   handle Promise.Force =>
      warn ("cyclic dependency: " ^
            (String.concatWith (!pending, " ")))
end

end
