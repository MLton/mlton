(* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PosixError: POSIX_ERROR_EXTRA =
   struct
      structure Prim = PrimitiveFFI.Posix.Error
      open Prim
      structure SysError = PrePosix.SysError

      type syserror = SysError.t

      val acces = SysError.fromRep EACCES
      val addrinuse = SysError.fromRep EADDRINUSE
      val addrnotavail = SysError.fromRep EADDRNOTAVAIL
      val afnosupport = SysError.fromRep EAFNOSUPPORT
      val again = SysError.fromRep EAGAIN
      val already = SysError.fromRep EALREADY
      val badf = SysError.fromRep EBADF
      val badmsg = SysError.fromRep EBADMSG
      val busy = SysError.fromRep EBUSY
      val canceled = SysError.fromRep ECANCELED
      val child = SysError.fromRep ECHILD
      val connaborted = SysError.fromRep ECONNABORTED
      val connrefused = SysError.fromRep ECONNREFUSED
      val connreset = SysError.fromRep ECONNRESET
      val deadlk = SysError.fromRep EDEADLK
      val destaddrreq = SysError.fromRep EDESTADDRREQ
      val dom = SysError.fromRep EDOM
      val dquot = SysError.fromRep EDQUOT
      val exist = SysError.fromRep EEXIST
      val fault = SysError.fromRep EFAULT
      val fbig = SysError.fromRep EFBIG
      val hostunreach = SysError.fromRep EHOSTUNREACH
      val idrm = SysError.fromRep EIDRM
      val ilseq = SysError.fromRep EILSEQ
      val inprogress = SysError.fromRep EINPROGRESS
      val intr = SysError.fromRep EINTR
      val inval = SysError.fromRep EINVAL
      val io = SysError.fromRep EIO
      val isconn = SysError.fromRep EISCONN
      val isdir = SysError.fromRep EISDIR
      val loop = SysError.fromRep ELOOP
      val mfile = SysError.fromRep EMFILE
      val mlink = SysError.fromRep EMLINK
      val msgsize = SysError.fromRep EMSGSIZE
      val multihop = SysError.fromRep EMULTIHOP
      val nametoolong = SysError.fromRep ENAMETOOLONG
      val netdown = SysError.fromRep ENETDOWN
      val netreset = SysError.fromRep ENETRESET
      val netunreach = SysError.fromRep ENETUNREACH
      val nfile = SysError.fromRep ENFILE
      val nobufs = SysError.fromRep ENOBUFS
      val nodata = SysError.fromRep ENODATA
      val nodev = SysError.fromRep ENODEV
      val noent = SysError.fromRep ENOENT
      val noexec = SysError.fromRep ENOEXEC
      val nolck = SysError.fromRep ENOLCK
      val nolink = SysError.fromRep ENOLINK
      val nomem = SysError.fromRep ENOMEM
      val nomsg = SysError.fromRep ENOMSG
      val noprotoopt = SysError.fromRep ENOPROTOOPT
      val nospc = SysError.fromRep ENOSPC
      val nosr = SysError.fromRep ENOSR
      val nostr = SysError.fromRep ENOSTR
      val nosys = SysError.fromRep ENOSYS
      val notconn = SysError.fromRep ENOTCONN
      val notdir = SysError.fromRep ENOTDIR
      val notempty = SysError.fromRep ENOTEMPTY
      val notsock = SysError.fromRep ENOTSOCK
      val notsup = SysError.fromRep ENOTSUP
      val notty = SysError.fromRep ENOTTY
      val nxio = SysError.fromRep ENXIO
      val opnotsupp = SysError.fromRep EOPNOTSUPP
      val overflow = SysError.fromRep EOVERFLOW
      val perm = SysError.fromRep EPERM
      val pipe = SysError.fromRep EPIPE
      val proto = SysError.fromRep EPROTO
      val protonosupport = SysError.fromRep EPROTONOSUPPORT
      val prototype = SysError.fromRep EPROTOTYPE
      val range = SysError.fromRep ERANGE
      val rofs = SysError.fromRep EROFS
      val spipe = SysError.fromRep ESPIPE
      val srch = SysError.fromRep ESRCH
      val stale = SysError.fromRep ESTALE
      val time = SysError.fromRep ETIME
      val timedout = SysError.fromRep ETIMEDOUT
      val toobig = SysError.fromRep E2BIG
      val txtbsy = SysError.fromRep ETXTBSY
      val wouldblock = SysError.fromRep EWOULDBLOCK
      val xdev = SysError.fromRep EXDEV

      local
         infixr 5 ::?
         fun (n,s) ::? l =
            if n = SysError.fromRep ~1
               then l
               else (n,s) :: l
      in
         val errorNames =
            (acces,"acces") ::?
            (addrinuse,"addrinuse") ::?
            (addrnotavail,"addrnotavail") ::?
            (afnosupport,"afnosupport") ::?
            (again,"again") ::?
            (already,"already") ::?
            (badf,"badf") ::?
            (badmsg,"badmsg") ::?
            (busy,"busy") ::?
            (canceled,"canceled") ::?
            (child,"child") ::?
            (connaborted,"connaborted") ::?
            (connrefused,"connrefused") ::?
            (connreset,"connreset") ::?
            (deadlk,"deadlk") ::?
            (destaddrreq,"destaddrreq") ::?
            (dom,"dom") ::?
            (dquot,"dquot") ::?
            (exist,"exist") ::?
            (fault,"fault") ::?
            (fbig,"fbig") ::?
            (hostunreach,"hostunreach") ::?
            (idrm,"idrm") ::?
            (ilseq,"ilseq") ::?
            (inprogress,"inprogress") ::?
            (intr,"intr") ::?
            (inval,"inval") ::?
            (io,"io") ::?
            (isconn,"isconn") ::?
            (isdir,"isdir") ::?
            (loop,"loop") ::?
            (mfile,"mfile") ::?
            (mlink,"mlink") ::?
            (msgsize,"msgsize") ::?
            (multihop,"multihop") ::?
            (nametoolong,"nametoolong") ::?
            (netdown,"netdown") ::?
            (netreset,"netreset") ::?
            (netunreach,"netunreach") ::?
            (nfile,"nfile") ::?
            (nobufs,"nobufs") ::?
            (nodata,"nodata") ::?
            (nodev,"nodev") ::?
            (noent,"noent") ::?
            (noexec,"noexec") ::?
            (nolck,"nolck") ::?
            (nolink,"nolink") ::?
            (nomem,"nomem") ::?
            (nomsg,"nomsg") ::?
            (noprotoopt,"noprotoopt") ::?
            (nospc,"nospc") ::?
            (nosr,"nosr") ::?
            (nostr,"nostr") ::?
            (nosys,"nosys") ::?
            (notconn,"notconn") ::?
            (notdir,"notdir") ::?
            (notempty,"notempty") ::?
            (notsock,"notsock") ::?
            (notsup,"notsup") ::?
            (notty,"notty") ::?
            (nxio,"nxio") ::?
            (opnotsupp,"opnotsupp") ::?
            (overflow,"overflow") ::?
            (perm,"perm") ::?
            (pipe,"pipe") ::?
            (proto,"proto") ::?
            (protonosupport,"protonosupport") ::?
            (prototype,"prototype") ::?
            (range,"range") ::?
            (rofs,"rofs") ::?
            (spipe,"spipe") ::?
            (srch,"srch") ::?
            (stale,"stale") ::?
            (time,"time") ::?
            (timedout,"timedout") ::?
            (toobig,"toobig") ::?
            (txtbsy,"txtbsy") ::?
            (wouldblock,"wouldblock") ::?
            (xdev,"xdev") ::?
            []
      end
      exception SysErr of string * syserror option

      val toWord = C_Int.castToSysWord o SysError.toRep
      val fromWord = SysError.fromRep o C_Int.castFromSysWord

      val cleared : syserror = SysError.fromRep 0

      fun errorName n =
         case List.find (fn (m, _) => n = m) errorNames of
            NONE => "<UNKNOWN>"
          | SOME (_, s) => s

      val _ =
         General.addExnMessager
         (fn e =>
          case e of
             SysErr (s, eo) =>
                SOME (concat ["SysErr: ", s,
                              case eo of
                                 NONE => ""
                               | SOME e => concat [" [", errorName e, "]"]])
           | _ => NONE)

      fun syserror s =
         case List.find (fn (_, s') => s = s') errorNames of
            NONE => NONE
          | SOME (n, _) => SOME n

      fun errorMsg (n: syserror) =
         let
            val cs = strError (SysError.toRep n)
         in
            if Primitive.MLton.Pointer.isNull 
               (Primitive.MLton.Pointer.fromWord cs)
               then "Unknown error"
               else CUtil.C_String.toString cs
         end

      fun raiseSys n = raise SysErr (errorMsg n, SOME n)
      fun raiseSysWithMsg (n, msg) = raise SysErr ((errorMsg n) ^ ": " ^ msg, SOME n)

      structure SysCall =
         struct
            structure Thread = Primitive.MLton.Thread

            val blocker: (unit -> (unit -> unit)) ref =
               ref (fn () => (fn () => ()))
               (* ref (fn () => raise Fail "blocker not installed") *)
            val restartFlag = ref true

            val syscallErr: {clear: bool, restart: bool, errVal: ''a} * 
                            (unit -> {return: ''a C_Errno.t,
                                      post: ''a -> 'b,
                                      handlers: (syserror * (unit -> 'b)) list}) -> 'b =
               fn ({clear, restart, errVal}, f) =>
               let
                  fun call (err: {errno: syserror,
                                  handlers: (syserror * (unit -> 'b)) list} -> 'b): 'b =
                     let
                        val () = Thread.atomicBegin ()
                        val () = if clear then clearErrno () else ()
                        val {return, post, handlers} = 
                           f () handle exn => (Thread.atomicEnd (); raise exn)
                        val return = C_Errno.check return
                     in
                        if errVal = return
                           then
                              (* Must getErrno () in the critical section. *)
                              let
                                 val e = SysError.fromRep (getErrno ())
                                 val () = Thread.atomicEnd ()
                              in
                                 err {errno = e, handlers = handlers}
                              end
                           else DynamicWind.wind (fn () => post return , Thread.atomicEnd)
                     end
                  fun err {default: unit -> 'b, 
                           errno: syserror, 
                           handlers: (syserror * (unit -> 'b)) list}: 'b =
                     case List.find (fn (e',_) => errno = e') handlers of
                        NONE => default ()
                      | SOME (_, handler) => handler ()
                  fun errBlocked {errno: syserror,
                                  handlers: (syserror * (unit -> 'b)) list}: 'b =
                     err {default = fn () => raiseSys errno,
                          errno = errno, handlers = handlers}
                  fun errUnblocked
                     {errno: syserror,
                      handlers: (syserror * (unit -> 'b)) list}: 'b =
                     err {default = fn () =>
                          if restart andalso errno = intr andalso !restartFlag
                             then if Thread.atomicState () = 0w0
                                     then call errUnblocked
                                     else let val finish = !blocker ()
                                          in 
                                             DynamicWind.wind
                                             (fn () => call errBlocked, finish)
                                          end
                             else raiseSys errno,
                          errno = errno, handlers = handlers}
               in
                  call errUnblocked
               end

            local
               val simpleResultAux = fn ({restart, errVal}, f) =>
                  syscallErr 
                  ({clear = false, restart = restart, errVal = errVal}, fn () => 
                   let val return = f () 
                   in {return = return, 
                       post = fn ret => ret, 
                       handlers = []}
                   end)
            in
               val simpleResultRestart = fn f =>
                  simpleResultAux ({restart = true, errVal = C_Int.fromInt ~1}, f)
               val simpleResult = fn f =>
                  simpleResultAux ({restart = false, errVal = C_Int.fromInt ~1}, f)

               val simpleResultRestart' = fn ({errVal}, f) =>
                  simpleResultAux ({restart = true, errVal = errVal}, f)
               val simpleResult' = fn ({errVal}, f) =>
                  simpleResultAux ({restart = false, errVal = errVal}, f)
            end

            val simpleRestart = ignore o simpleResultRestart
            val simple = ignore o simpleResult

            val simpleRestart' = fn ({errVal}, f) => 
               ignore (simpleResultRestart' ({errVal = errVal}, f))
            val simple' = fn ({errVal}, f) => 
               ignore (simpleResult' ({errVal = errVal}, f))

            val syscallRestart' = fn ({errVal}, f) => 
               syscallErr 
               ({clear = false, restart = true, errVal = errVal}, fn () => 
                let val (return, post) = f () 
                in {return = return, post = post, handlers = []}
                end)
            val syscall' = fn ({errVal}, f) =>
               syscallErr 
               ({clear = false, restart = false, errVal = errVal}, fn () => 
                let val (return, post) = f () 
                in {return = return, post = post, handlers = []}
                end)
            val syscallRestart = fn f => 
               syscallRestart' ({errVal = C_Int.fromInt ~1}, f)
            val syscall = fn f => 
               syscall' ({errVal = C_Int.fromInt ~1}, f)
         end
   end
