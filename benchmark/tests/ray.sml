(* From the SML/NJ benchmark suite. *)

(* objects.sml
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * Type declarations for the various objects in the ray tracer.
 *)

structure Objects =
  struct

    datatype point = PT of {x : real, y : real, z : real}

    datatype vector = VEC of {l : real, m : real, n : real}

    datatype ray = Ray of {s : point, d : vector}

    datatype camera = Camera of {
        vp : point,
        ul : point,
        ur : point,
        ll : point,
        lr : point
      }

    datatype color = Color of {red : real, grn : real, blu : real}

    datatype sphere = Sphere of {c : point, r : real, color : color}

    datatype hit = Miss | Hit of {t : real, s : sphere}

    datatype visible = Visible of {h : point, s : sphere}

    datatype object
      = TOP
      | NUMBER of real
      | NAME of string
      | LIST of object list
      | OPERATOR of object list -> object list
      | MARK
      | LITERAL of string
      | UNMARK
      | POINT of point
      | VECTOR of vector
      | RAY of ray
      | CAMERA of camera
      | COLOR of color
      | SPHERE of sphere
      | HIT
      | VISIBLE

  end (* Objects *)
(* interp.sml
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *)

structure Interp =
  struct

    local
      val exit = OS.Process.exit
      fun ordof(s, i) = Char.ord(String.sub(s, i))
      exception NotAChar
      exception NotAReal
      fun fromStr x = 
        (case Char.fromString x
          of SOME c => c
           | NONE => raise NotAChar)

     fun strToReal s = 
      (case Real.fromString s
        of SOME r => r
        | _ => raise NotAReal)

    fun intToReal x = 
     (strToReal ((Int.toString x) ^ ".0"))


      val explode = (fn x => map Char.toString (explode x))
      val implode = (fn x => implode (map fromStr x))

      open Objects
      val dict = ref ([] : {key : string, value : object} list)
      fun dictInsert (NAME key, value) = let
            fun find [] = [{key=key, value=value}]
              | find (x::r) = if (key = #key x)
                  then {key=key, value=value}::r
                  else x :: (find r)
            in
              dict := find(!dict)
            end
        | dictInsert _ = raise Fail "dictInsert"
      fun prObj outStrm obj = let
            fun printf args = TextIO.output(outStrm, implode args)
            fun pr (NUMBER n) = printf["  ", Real.toString n, "\n"]
              | pr (NAME s) = printf["  ",  s, "\n"]
              | pr (LITERAL s) = printf["  ", s, "\n"]
              | pr (LIST l) = app pr l
              | pr MARK = printf["  MARK\n"]
              | pr (OPERATOR _) = printf["  <operator>\n"]
              | pr TOP = printf["  TOP OF STACK\n"]
              | pr _ = printf["  <object>\n"]
            in
              pr obj
            end
    in

    exception Stop

    fun error opName stk = let
          fun prStk ([], _) = ()
            | prStk (_, 0) = ()
            | prStk (obj::r, i) = (prObj TextIO.stdErr obj; prStk(r, i-1))
          in
            TextIO.output(TextIO.stdErr, "ERROR: "^opName^"\n");
            prStk (stk, 10);
            raise (Fail opName)
          end

    fun installOperator (name, rator) =
          dictInsert (NAME name, OPERATOR rator)

    fun ps_def (v::k::r) = (dictInsert(k, v); r)
      | ps_def stk = error "ps_def" stk

    local
      fun binOp (f, opName) = let
            fun g ((NUMBER arg1)::(NUMBER arg2)::r) =
                  NUMBER(f(arg2, arg1)) :: r
              | g stk = error opName stk
            in
              g
            end
    in
    val ps_add = binOp (op +, "add")
    val ps_sub = binOp (op -, "sub")
    val ps_mul = binOp (op *, "mul")
    val ps_div = binOp (op /, "div")
    end

    fun ps_rand stk = (NUMBER 0.5)::stk (** ??? **)

    fun ps_print (obj::r) = (prObj TextIO.stdOut obj; r)
      | ps_print stk = error "print" stk

    fun ps_dup (obj::r) = (obj::obj::r)
      | ps_dup stk = error "dup" stk

    fun ps_stop _ = raise Stop

  (* initialize dictionary and begin parsing input *)
    fun parse inStrm = let
          fun getc () = case TextIO.input1 inStrm of NONE => ""
                               | SOME c => Char.toString c
          fun peek () = case TextIO.lookahead inStrm
                         of SOME x => Char.toString x
                          | _ => ""
        (* parse one token from inStrm *)
          fun toke deferred = let
                fun doChar "" = exit OS.Process.success
                  | doChar "%" = let
                      fun lp "\n" = doChar(getc())
                        | lp "" = exit OS.Process.success
                        | lp _ = lp(getc())
                      in
                        lp(getc())
                      end
                  | doChar "{" = (MARK, deferred+1)
                  | doChar "}" = (UNMARK, deferred-1)
                  | doChar c = if Char.isSpace (fromStr c)
                      then doChar(getc())
                      else let
                        fun lp buf = (case peek()
                               of "{" => buf
                                | "}" => buf
                                | "%" => buf
                                | c => if Char.isSpace(fromStr c)
                                    then buf
                                    else (getc(); lp(c::buf))
                              (* end case *))
                        val tok = implode (rev (lp [c]))
                        val hd = ordof(tok, 0)
                        in
                          if (hd = ord (#"/"))
                            then (LITERAL(substring(tok, 1, size tok - 1)), deferred)
                          else 
                            if ((Char.isDigit (chr hd)) orelse (hd = ord (#"-")))
                            then (NUMBER(strToReal(tok)), deferred)
                            else (NAME tok, deferred)
                        end
                in
                  doChar(getc())
                end
        (* execute a token (if not deferred) *)
          fun exec (UNMARK, stk, _) = let
                fun lp ([], _) = raise Fail "MARK"
                  | lp (MARK::r, l) = (LIST l)::r
                  | lp (x::r, l) = lp (r, x::l)
                  in
                    lp (stk, [])
                  end
            | exec (OPERATOR f, stk, 0) = f stk
            | exec (LIST l, stk, 0) = let
                fun execBody ([], stk) = stk
                  | execBody (obj::r, stk) = (exec(obj, stk, 0); execBody(r, stk))
                in
                  execBody (l, stk)
                end
            | exec (NAME s, stk, 0) = let
                fun find [] = raise Fail "undefined name"
                  | find ({key, value}::r) = if (key = s) then value else find r
                in
                  exec (find (!dict), stk, 0)
                end
            | exec (obj, stk, _) = obj::stk
          fun lp (stk, level) = let
                val (obj, level) = toke level
                val stk = exec (obj, stk, level)
                in
                  lp (stk, level)
                end
          in
            installOperator ("add", ps_add);
            installOperator ("def", ps_def);
            installOperator ("div", ps_div);
            installOperator ("dup", ps_dup);
            installOperator ("mul", ps_mul);
            installOperator ("print", ps_print);
            installOperator ("rand", ps_rand);
            installOperator ("stop", ps_stop);
            installOperator ("sub", ps_sub);
            (lp ([], 0)) handle Stop => ()
          end (* parse *)

    end (* local *)

  end (* Interp *)
(* ray.sml
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *)

structure Ray =
  struct
    local open Objects in

  (** basic operations on points and vectors **)

    fun scaleVector (s, VEC{l, m, n}) = VEC{l=s*l, m=s*m, n=s*n}

    fun vecPlusVec (VEC{l, m, n}, VEC{l=l', m=m', n=n'}) = VEC{l=l+l', m=m+m', n=n+n'}

    fun vecPlusPt (VEC{l, m, n}, PT{x, y, z}) = PT{x=x+l, y=y+m, z=z+n}

    fun ptMinusPt (PT{x, y, z}, PT{x=x', y=y', z=z'}) = VEC{l=x-x', m=y-y', n=z-z'}

    fun wave (PT{x, y, z}, PT{x=x', y=y', z=z'}, w) = PT{
            x = w * (x' - x) + x,
            y = w * (y' - y) + y,
            z = w * (z' - z) + z
          }

    fun dotProd (VEC{l, m, n}, VEC{l=l', m=m', n=n'}) = ((l*l') + (m*m') + (n*n'))

  (* normal vector to sphere *)
    fun normalSphere (Visible{h, s as Sphere{c, ...}}) = let
          val n = ptMinusPt(h, c)
          val norm = Math.sqrt(dotProd(n, n))
          in
            scaleVector(1.0 / norm, n)
          end

  (* intersect a ray with a sphere *)
    fun intersectSphere (Ray ray, s as Sphere sphere) = let
          val a = dotProd(#d ray, #d ray)
          val sdiffc = ptMinusPt(#s ray, #c sphere)
          val b = 2.0 * dotProd(sdiffc, #d ray)
          val c = dotProd(sdiffc, sdiffc) - (#r sphere * #r sphere)
          val d = b*b - 4.0*a*c
          in
            if (d <= 0.0)
              then Miss
              else let
                val d = Math.sqrt(d)
                val t1 = (~b - d) / (2.0 * a)
                val t2 = (~b + d) / (2.0 * a)
                val t = if ((t1 > 0.0) andalso (t1 < t2)) then t1 else t2
                in
                  Hit{t=t, s=s}
                end
          end

  (* simple shading function *)
    fun shade {light, phi} (visible as Visible{h, s}) = let
          val l = ptMinusPt(light, h)
          val n = normalSphere(visible)
          val irradiance = phi * dotProd(l,n) / dotProd(l,l);
          val irradiance = (if (irradiance < 0.0) then 0.0 else irradiance) + 0.05
          val Sphere{color=Color{red, grn, blu}, ...} = s
          in
            Color{red=red*irradiance, grn=grn*irradiance, blu=blu*irradiance}
          end

    fun trace (ray as (Ray ray'), objList) = let
          fun closest (Miss, x) = x
            | closest (x, Miss) = x
            | closest (h1 as Hit{t=t1, ...}, h2 as Hit{t=t2, ...}) =
                if (t2 < t1) then h2 else h1
          fun lp ([], Hit{t, s}) = Visible{
                  h = vecPlusPt(scaleVector(t, #d ray'), #s ray'),
                  s = s
                }
            | lp (s :: r, closestHit) =
                lp (r, closest (closestHit, intersectSphere (ray, s)))
            | lp _ = raise Fail "trace"
          in
            lp (objList, Miss)
          end

    fun camera (Camera cam) (x, y) = let
          val l = wave (#ul cam, #ll cam, y)
          val r = wave (#ur cam, #lr cam, y)
          val image_point = wave(l, r, x)
          in
            Ray{d = ptMinusPt(image_point, #vp cam), s = #vp cam}
          end

    val shade = shade {light = PT{x = 10.0, y = ~10.0, z = ~10.0}, phi = 16.0}
    val camera = camera (Camera{
            vp = PT{x = 0.0, y = 0.0, z = ~3.0},
            ul = PT{x = ~1.0, y = ~1.0, z = 0.0},
            ur = PT{x = 1.0, y = ~1.0, z = 0.0},
            ll = PT{x = ~1.0, y = 1.0, z = 0.0},
            lr = PT{x = 1.0, y = 1.0, z = 0.0}
          })

    fun image objList (x, y) = shade (trace(camera(x, y), objList))

    fun picture (picName, objList) = let
          val outStrm = TextIO.openOut picName
          val image = image objList
          val print = fn x => TextIO.output (outStrm, x)
          fun putc c = TextIO.output1(outStrm, chr c)
          fun doPixel (i, j) = let
                val x = (real i) / 512.0
                val y = (real j) / 512.0
                val (Color c) = image (x, y)
                fun cvt x = if (x >= 1.0) then 255 else floor(256.0*x)
                in
                  putc (cvt (#red c));
                  putc (cvt (#grn c));
                  putc (cvt (#blu c))
                end
          fun lp_j j = if (j < 512)
                then let
                  fun lp_i i = if (i < 512)
                        then (doPixel(i, j); lp_i(i+1))
                        else ()
                  in
                    lp_i 0; lp_j(j+1)
                  end
                else ()
          in
            print "TYPE=dump\n";
            print "WINDOW=0 0 512 512\n";
            print "NCHAN=3\n";
            print "CHAN=rgb\n";
            print "\n";
            lp_j 0;
            TextIO.closeOut outStrm
          end

    end (* local *)
  end; (* Ray *)
(* interface.sml
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * The interface between the interpreter and the ray tracer.
 *)

structure Interface =
  struct
    local
      open Objects
    in

  (* color pops three numbers and pushes a color object.
   * usage: red-value green-value blue-value color
   *)
    fun ps_color ((NUMBER blu)::(NUMBER grn)::(NUMBER red)::r) =
          (COLOR(Color{red=red, grn=grn, blu=blu})) :: r
      | ps_color stk = Interp.error "color" stk

  (* pop radius, coordinates of center, and a color and push a sphere
   * usage: radius x y z color-value sphere
   *)
    fun ps_sphere (
          (COLOR c)::(NUMBER z)::(NUMBER y)::(NUMBER x)::(NUMBER rad)::r
        ) = SPHERE(Sphere{c=PT{x=x, y=y, z=z}, r=rad, color=c}) :: r
      | ps_sphere stk = Interp.error "sphere" stk

  (* build an object list from solids on the stack, then invoke raytracer *)
    fun ps_raytrace ((LITERAL picName)::r) = let
          fun mkObjList ([], l) = l
            | mkObjList ((SPHERE s)::r, l) = mkObjList(r, s::l)
            | mkObjList (_::r, l) = mkObjList(r, l)
          in
            Ray.picture(picName, mkObjList(r, []));
            []
          end
      | ps_raytrace stk = Interp.error "raytrace" stk

  (* add ray tracing operations to interpreter dictionary *)
    fun rtInit () = (
          Interp.installOperator("color", ps_color);
          Interp.installOperator("sphere", ps_sphere);
          Interp.installOperator("raytrace", ps_raytrace))

    end (* local *)
  end;

signature BMARK =
  sig
    val doit : int -> unit
    val testit : TextIO.outstream -> unit
  end;
(* main.sml
 *
 * COPYRIGHT (c) 1992 AT&T Bell Laboratories
 *
 * Main structure for running raytracer as benchmark.
 *)

structure Main : BMARK =
  struct

    fun doit n =
       let
          fun loop n =
             if n = 0
                then ()
             else
                let
                   val strm = TextIO.openIn "DATA/ray"
                   val _ = Interface.rtInit()
                   val _ = Interp.parse strm
                   val _ = TextIO.closeIn strm
                in
                   loop (n - 1)
                end
       in
          loop n
       end

    fun testit _ = ()
  end
