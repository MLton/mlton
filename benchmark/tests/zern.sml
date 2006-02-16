(*
 * From David McClain's language study.
 * http://www.azstarnet.com/~dmcclain/LanguageStudy.html
 *
 * Stephen Weeks replaced Unsafe.Real64Array with Real64Array.
 *)

fun print _ = ()
   
(* array2.sml
 *
 * COPYRIGHT (c) 1998 D.McClain/MCFA
 * COPYRIGHT (c) 1997 AT&T Research.
 *)

structure FastRealArray2 :
    sig
        type array

        type region
            = {base : array,
               row : int,
               col : int,
               nrows : int option,
               ncols : int option}

        datatype traversal = RowMajor | ColMajor

        val array : int * int * real -> array
        val fromList : real list list -> array
        val tabulate : traversal -> (int * int * (int * int -> real)) -> array
        val sub : array * int * int -> real
        val update : array * int * int * real -> unit
        val dimensions : array -> int * int
        val size : array -> int
        val nCols : array -> int
        val nRows : array -> int
        val row : array * int -> real Vector.vector
        val column : array * int -> real Vector.vector

        val copy : region * array * int * int -> unit
        val appi : traversal -> (int * int * real -> unit) -> region -> unit
        val app : traversal -> (real -> unit) -> array -> unit
        val modifyi : traversal -> (int * int * real -> real) -> region -> unit
        val modify : traversal -> (real -> real) -> array -> unit
        val foldi : traversal -> (int*int*real*'a -> 'a) -> 'a -> region -> 'a
        val fold : traversal -> (real * 'a -> 'a) -> 'a -> array -> 'a

        val rmSub : array * int -> real
        val rmUpdate : array * int * real -> unit

        val unop : array * array * (real -> real) -> unit
        val unopi : array * array * (real * int -> real) -> unit
        val binop : array * array * array * (real * real -> real) -> unit
        val binopi : array * array * array * (real * real * int -> real) -> unit
        val fill : array * real -> unit
        val fillf : array * (int -> real) -> unit
            
        val transpose : array -> array
        val extract : region -> array

        (*
        val shift : array * int * int -> array
        *)
    end =
  struct

    structure A      = (*Unsafe.*)Real64Array

    type rawArray    = A.array
        
    val unsafeUpdate = A.update
    val unsafeSub    = A.sub
    fun mkRawArray n = A.array (n, 0.0)

        
    type array = {data  : rawArray,
                  nrows : int,
                  ncols : int,
                  nelts : int}
        
    type region = {base  : array,
                   row   : int,
                   col   : int,
                   nrows : int option,
                   ncols : int option}

    datatype traversal = RowMajor | ColMajor

        
    fun dotimes n f =
        let (* going forward is twice as fast as backward! *)
            fun iter k = if k >= n then ()
                         else (f(k); iter(k+1))
        in
            iter 0
        end

    
    fun mkArray(n,v) =
        let
            val arr = mkRawArray n
        in
            dotimes n (fn ix => unsafeUpdate(arr,ix,v));
            arr
        end

  (* compute the index of an array element *)
    fun ltu(i,limit) = (i >= 0) andalso (i < limit)
    fun unsafeIndex ({nrows, ncols, ...} : array, i, j) = (i*ncols + j)
    fun index (arr, i, j) =
          if (ltu(i, #nrows arr) andalso ltu(j, #ncols arr))
            then unsafeIndex (arr, i, j)
            else raise General.Subscript
    (* row major index checking *)
    fun rmIndex ({nelts,...}: array, ix) =
        if ltu(ix, nelts) then ix
        else raise General.Subscript

    val max_length = 4096 * 4096; (* arbitrary - but this is 128 MB *)
        
    fun chkSize (nrows, ncols) =
          if (nrows <= 0) orelse (ncols <= 0)
            then raise General.Size
            else let
              val n = nrows*ncols handle Overflow => raise General.Size
              in
                if (max_length < n) then raise General.Size else n
              end

    fun array (nrows, ncols, v) =
        let
            val nelts = chkSize (nrows, ncols)
        in
            {data = mkArray (nelts, v),
             nrows = nrows, ncols = ncols, nelts = nelts}
        end

    fun fromList [] = raise General.Size
      | fromList (row1 :: rest) = let
          val ncols = List.length row1
          fun chk ([], nrows, l) = (nrows, l)
            | chk (row::rest, nrows, l) = let
                fun chkRow ([], n, revCol) = (
                      if (n <> ncols) then raise General.Size else ();
                      List.revAppend (revCol, l))
                  | chkRow (x::r, n, revCol) = chkRow (r, n+1, x::revCol)
                in
                  chk (rest, nrows+1, chkRow(row, 0, []))
                end
          val (nrows, flatList) = chk (rest, 1, [])
          val nelts = chkSize(nrows, ncols)
          val arr = mkRawArray nelts
          fun upd(_,nil) = arr
            | upd(k,v::vs) = (unsafeUpdate(arr,k,v); upd(k+1,vs))
          in
            { data = upd(0,List.@(row1, flatList)),
              nrows = nrows,
              ncols = ncols,
              nelts = nelts }
          end
      
    fun tabulateRM (nrows, ncols, f) =
        let
            val nelts = chkSize(nrows, ncols)
            val arr = mkRawArray nelts
            fun lp1 (i, j, k) = if (i < nrows)
                                    then lp2 (i, 0, k)
                                else ()
            and lp2 (i, j, k) = if (j < ncols)
                                    then (
                                          unsafeUpdate(arr, k, f(i, j));
                                          lp2 (i, j+1, k+1))
                                else lp1 (i+1, 0, k)
        in
            lp2 (0, 0, 0);
            {data = arr, nrows = nrows, ncols = ncols, nelts = nelts}
        end

    fun tabulateCM (nrows, ncols, f) =
        let
            val nelts   = chkSize(nrows,ncols)
            val arr = mkRawArray nelts
            val delta = nelts - 1
            fun lp1 (i, j, k) = if (j < ncols)
                                    then lp2 (0, j, k)
                                else ()
            and lp2 (i, j, k) = if (i < nrows)
                                    then (
                                          unsafeUpdate(arr, k, f(i, j));
                                          lp2 (i+1, j, k+ncols))
                                else lp1 (0, j+1, k-delta)
        in
            lp2 (0, 0, 0);
            {data = arr, nrows = nrows, ncols = ncols, nelts = nelts}
        end

    fun tabulate RowMajor = tabulateRM
      | tabulate ColMajor = tabulateCM
        
    fun sub (a, i, j) = unsafeSub(#data a, index(a, i, j))
    fun update (a, i, j, v) = unsafeUpdate(#data a, index(a, i, j), v)
    fun dimensions ({nrows, ncols, ...}: array) = (nrows, ncols)
    fun size ({nelts,...}: array) = nelts
    fun nCols (arr : array) = #ncols arr
    fun nRows (arr : array) = #nrows arr
    fun row ({data, nrows, ncols, ...}: array, i) =
        if ltu(i, nrows) then
            let
                val stop = i*ncols
                fun mkVec (j, l) =
                    if (j < stop)
                        then Vector.fromList l
                    else mkVec(j-1, unsafeSub(data, j)::l)
            in
                if ltu(nrows, i)
                    then raise General.Subscript
                else mkVec (stop+ncols-1, [])
            end
        else raise General.Subscript
    fun column ({data, ncols, nelts, ...}: array, j) =
        if ltu(j, ncols) then
            let
                fun mkVec (i, l) =
                    if (i < 0)
                        then Vector.fromList l
                    else mkVec(i-ncols, unsafeSub(data, i)::l)
            in
                if ltu(ncols, j)
                    then raise General.Subscript
                else mkVec ((nelts - ncols) + j, [])
            end
        else raise General.Subscript
            
    datatype index = DONE | INDX of {i:int, r:int, c:int}

    fun chkRegion {base={data, nrows, ncols, ...}: array,
                   row, col, nrows=nr, ncols=nc}
        = let
          fun chk (start, n, NONE) =
                if ((start < 0) orelse (n < start))
                  then raise General.Subscript
                  else n-start
            | chk (start, n, SOME len) =
                if ((start < 0) orelse (len < 0) orelse (n < start+len))
                  then raise General.Subscript
                  else len
          val nr = chk (row, nrows, nr)
          val nc = chk (col, ncols, nc)
          in
            {data = data, i = (row*ncols + col), r=row, c=col, nr=nr, nc=nc}
          end

    fun copy (region, dst, dst_row, dst_col) =
          raise Fail "Array2.copy unimplemented"


  (* this function generates a stream of indices for the given region in
   * row-major order.
   *)
    fun iterateRM arg = let
          val {data, i, r, c, nr, nc} = chkRegion arg
          val ii = ref i and ri = ref r and ci = ref c
          fun mkIndx (r, c) = let val i = !ii
                in
                  ii := i+1;
                  INDX{i=i, c=c, r=r}
                end
          fun iter () = let
                val r = !ri and c = !ci
                in
                  if (c < nc)
                    then (ci := c+1; mkIndx(r, c))
                  else if (r+1 < nr)
                    then (ci := 0; ri := r+1; iter())
                    else DONE
                end
          in
            (data, iter)
          end

  (* this function generates a stream of indices for the given region in
   * col-major order.
   *)
    fun iterateCM (arg as {base={ncols, ...}, ...}) = let
          val {data, i, r, c, nr, nc} = chkRegion arg
          val delta = nr * ncols - 1
          val ii = ref i and ri = ref r and ci = ref c
          fun mkIndx (r, c) = let val i = !ii
                in
                  ii := i+ncols;
                  INDX{i=i, c=c, r=r}
                end
          fun iter () = let
                val r = !ri and c = !ci
                in
                  if (r < nr)
                    then (ri := r+1; mkIndx(r, c))
                  else if (c+1 < nc)
                    then (ii := !ii-delta; ri := 0; ci := c+1; iter())
                    else DONE
                end
          in
            (data, iter)
          end

    fun appi order f region = let
          val (data, iter) = (case order
                 of RowMajor => iterateRM region
                  | ColMajor => iterateCM region
                (* end case *))
          fun app () = (case iter()
                 of DONE => ()
                  | INDX{i, r, c} => (f(r, c, unsafeSub(data, i)); app())
                (* end case *))
          in
            app ()
          end

    fun appRM f ({data, nelts, ...}: array) =
        let
            fun appf k =
                if k < nelts then (f(unsafeSub(data,k));
                                   appf(k+1))
                else ()
        in
            appf 0
        end
    
    fun appCM f {data, ncols, nrows, nelts} = let
          val delta = nelts - 1
          fun appf (i, k) = if (i < nrows)
                then (f(unsafeSub(data, k)); appf(i+1, k+ncols))
                else let
                  val k = k-delta
                  in
                    if (k < ncols) then appf (0, k) else ()
                  end
          in
            appf (0, 0)
          end
    fun app RowMajor = appRM
      | app ColMajor = appCM

    fun modifyi order f region = let
          val (data, iter) = (case order
                 of RowMajor => iterateRM region
                  | ColMajor => iterateCM region
                (* end case *))
          fun modify () = (case iter()
                 of DONE => ()
                  | INDX{i, r, c} => (
                      unsafeUpdate (data, i, f(r, c, unsafeSub(data, i)));
                      modify())
                (* end case *))
          in
            modify ()
          end

    fun modifyRM f ({data, nelts, ...}: array) =
        let
            fun modf k =
                if k < nelts then (unsafeUpdate(data,k,f(unsafeSub(data,k)));
                               modf (k+1))
                    else ()
        in
            modf 0
        end
    
    fun modifyCM f {data, ncols, nrows, nelts} = let
          val delta = nelts - 1
          fun modf (i, k) = if (i < nrows)
                then (unsafeUpdate(data, k, f(unsafeSub(data, k))); modf(i+1, k+ncols))
                else let
                  val k = k-delta
                  in
                    if (k < ncols) then modf (0, k) else ()
                  end
          in
            modf (0, 0)
          end
    fun modify RowMajor = modifyRM
      | modify ColMajor = modifyCM

    fun foldi order f init region = let
          val (data, iter) = (case order
                 of RowMajor => iterateRM region
                  | ColMajor => iterateCM region
                (* end case *))
          fun fold accum = (case iter()
                 of DONE => accum
                  | INDX{i, r, c} => fold(f(r, c, unsafeSub(data, i), accum))
                (* end case *))
          in
            fold init
          end

    fun foldRM f init ({data, nelts, ...}: array) =
        let
            fun foldf (k, accum) =
                if k < nelts then foldf(k+1,f(unsafeSub(data,k),accum))
                else accum
        in
            foldf (0,init)
        end

    fun foldCM f init {data, ncols, nrows, nelts} = let
          val delta = nelts - 1
          fun foldf (i, k, accum) = if (i < nrows)
                then foldf (i+1, k+ncols, f(unsafeSub(data, k), accum))
                else let
                  val k = k-delta
                  in
                    if (k < ncols) then foldf (0, k, accum) else accum
                  end
          in
            foldf (0, 0, init)
          end
    fun fold RowMajor = foldRM
      | fold ColMajor = foldCM


    fun transpose {data, nrows, ncols, nelts} =
        let
            val dst = mkRawArray nelts
            val delta = nelts - 1
            fun iter (k,k') =
                if k >= nelts then {data = dst,
                                    nrows = ncols,
                                    ncols = nrows,
                                    nelts = nelts}
                else (if k' >= nelts then iter(k,k' - delta)
                      else (unsafeUpdate(dst,k',unsafeSub(data,k));
                            iter(k+1,k'+nrows)))
        in
            iter(0,0)
        end

    fun extract (region as {base,row,col,nrows,ncols}) =
        let
            fun chk (start,limit,NONE) =
                if ltu(start,limit) then limit - start
                else raise General.Subscript

              | chk (start, limit, SOME len) =
                if ltu(start + len - 1, limit) then len
                else raise General.Subscript

            val nr = chk(row, nRows(base), nrows)
            val nc = chk(col, nCols(base), ncols)
            val n = nr * nc
            val dst = mkRawArray n
            val (data, iter) = iterateRM region
            fun app (k) = (case iter() of
                              DONE => {data  = dst,
                                       nrows = nr,
                                       ncols = nc,
                                       nelts = n}
                            | INDX{i,...} =>
                                  (unsafeUpdate(dst,k,unsafeSub(data,i));
                                   app(k+1)))
        in
            app (0)
        end

    fun rmSub (arr as {data,...}: array,ix) =
        unsafeSub(data,rmIndex(arr, ix))

    fun rmUpdate(arr as {data,...}: array,ix,v) =
        unsafeUpdate(data,rmIndex(arr, ix),v)

    fun binop ({data=dst,nelts=nelts,...}: array,
               {data=src1,...}: array,
               {data=src2,...}: array,
               f) =
        dotimes nelts
        (fn (ix) => unsafeUpdate(dst,ix,f(unsafeSub(src1,ix),
                                          unsafeSub(src2,ix))))

    fun unop ({data=dst,nelts=nelts,...}: array,
              {data=src,...}: array,
               f) =
        dotimes nelts
        (fn (ix) => unsafeUpdate(dst,ix,f(unsafeSub(src,ix))))

    fun binopi ({data=dst,nelts=nelts,...}: array,
                {data=src1,...}: array,
                {data=src2,...}: array,
                f) =
        dotimes nelts
        (fn ix => unsafeUpdate(dst,ix,f(unsafeSub(src1,ix),
                                        unsafeSub(src2,ix),
                                        ix)))

    fun unopi ({data=dst,nelts=nelts,...}: array,
               {data=src,...}: array,
               f) =
        dotimes nelts
        (fn ix => unsafeUpdate(dst,ix,f(unsafeSub(src,ix),ix)))
        
    fun fill ({data=dst,nelts=nelts,...}: array,v) =
        dotimes nelts
        (fn ix => unsafeUpdate(dst,ix,v))
         
    fun fillf ({data=dst,nelts=nelts,...}: array,f) =
        dotimes nelts
        (fn ix => unsafeUpdate(dst,ix,f(ix)))

  end

(* test of Zernick phase screen E-field generation *)
(* This is 1.9 times faster than IDL!!!! *)
structure MSpeed =
    struct
        structure F = FastRealArray2
            
        val sin = Math.sin
        val cos = Math.cos
            
        val fromInt = LargeReal.fromInt

        (* setup working vectors and arrays *)
        fun collect n f =
            let
                fun g 0 l = l
                  | g n l = g (n-1) ((f n) :: l)
            in
                g n nil
            end

        val ncoefs = 15
        val nx     = 128
        val ny     = nx
        val nel    = nx * ny

        (* generate an array from a scaled vector *)
        fun mulsv (dst, sf, a) =
            F.unop(dst,a,fn(vsrc) => sf * vsrc)

            
        (* compute the complex exponential of an array *)
        fun cisv (a, rpart, ipart) =
            (F.unop(rpart,a,cos);
             F.unop(ipart,a,sin);
             (rpart,ipart))

        (* accumulate scaled vectors into an array *)
        fun mpadd dst (sf, src) =
            F.binop(dst,dst,src,fn(vdst,vsrc) => vdst + sf * vsrc)

            
        (* compute an E-field from a set of Zernike screens *)
        fun zern (dst, rpart, ipart, coefs, zerns) =
            (mulsv (dst, hd coefs, hd zerns);
             ListPair.app (mpadd dst) (tl coefs, tl zerns);
             cisv (dst, rpart, ipart))
            
        (* timing tests and reporting *)
        fun report_times(niter, nel, (start, stop)) =
            let
                val secs = Time.-(stop,start)
                val dur  = Time.toReal(secs) * 1.0E6
                val ops_per_us = ((fromInt niter) * (fromInt nel)) / dur
                val ns_per_op = 1000.0 / ops_per_us
            in
                print(Time.toString (Time.-(stop,start)));
                print("\n");
                { ops_per_us = ops_per_us, ns_per_op = ns_per_op}
            end

        fun time_iterations f niter =
            let
                fun iter 0 = Time.now()
                  | iter n = (ignore (f()); iter (n-1))
            in
                (Time.now(), iter niter)
            end

        fun ztest niter =
            report_times(niter, nel,
                         time_iterations
                         (fn () =>
                          let val sum   = F.array(ny,nx, 0.0)
                             val rpart = F.array(ny,nx, 0.0)
                             val ipart = F.array(ny,nx, 0.0)                
                             val coefs = collect ncoefs (fn(x) => real(1 + x))
                             val zerns =
                                collect ncoefs
                                (fn(x) => F.tabulate F.RowMajor
                                 (ny, nx, fn(r,c) => 0.01 * real(nx * r + c)))
                             val (rpart, _) =
                                zern (sum, rpart, ipart, coefs, zerns)
                          in if Real.abs(FastRealArray2.sub(rpart, 0, 1) - 0.219)
                                 < 0.001
                                then ()
                             else raise Fail "compiler bug"
                          end)
                         niter)
end

structure Main =
   struct
      fun doit n = MSpeed.ztest n
   end
