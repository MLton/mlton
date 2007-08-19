(* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PosixError: POSIX_ERROR_EXTRA =
   struct
      structure Prim = PrimitiveFFI.Posix.Error
      open Prim

      type syserror = C_Int.t

      val acces = EACCES
      val addrinuse = EADDRINUSE
      val addrnotavail = EADDRNOTAVAIL
      val afnosupport = EAFNOSUPPORT
      val again = EAGAIN
      val already = EALREADY
      val badf = EBADF
      val badmsg = EBADMSG
      val busy = EBUSY
      val canceled = ECANCELED
      val child = ECHILD
      val connaborted = ECONNABORTED
      val connrefused = ECONNREFUSED
      val connreset = ECONNRESET
      val deadlk = EDEADLK
      val destaddrreq = EDESTADDRREQ
      val dom = EDOM
      val dquot = EDQUOT
      val exist = EEXIST
      val fault = EFAULT
      val fbig = EFBIG
      val hostunreach = EHOSTUNREACH
      val idrm = EIDRM
      val ilseq = EILSEQ
      val inprogress = EINPROGRESS
      val intr = EINTR
      val inval = EINVAL
      val io = EIO
      val isconn = EISCONN
      val isdir = EISDIR
      val loop = ELOOP
      val mfile = EMFILE
      val mlink = EMLINK
      val msgsize = EMSGSIZE
      val multihop = EMULTIHOP
      val nametoolong = ENAMETOOLONG
      val netdown = ENETDOWN
      val netreset = ENETRESET
      val netunreach = ENETUNREACH
      val nfile = ENFILE
      val nobufs = ENOBUFS
      val nodata = ENODATA
      val nodev = ENODEV
      val noent = ENOENT
      val noexec = ENOEXEC
      val nolck = ENOLCK
      val nolink = ENOLINK
      val nomem = ENOMEM
      val nomsg = ENOMSG
      val noprotoopt = ENOPROTOOPT
      val nospc = ENOSPC
      val nosr = ENOSR
      val nostr = ENOSTR
      val nosys = ENOSYS
      val notconn = ENOTCONN
      val notdir = ENOTDIR
      val notempty = ENOTEMPTY
      val notsock = ENOTSOCK
      val notsup = ENOTSUP
      val notty = ENOTTY
      val nxio = ENXIO
      val opnotsupp = EOPNOTSUPP
      val overflow = EOVERFLOW
      val perm = EPERM
      val pipe = EPIPE
      val proto = EPROTO
      val protonosupport = EPROTONOSUPPORT
      val prototype = EPROTOTYPE
      val range = ERANGE
      val rofs = EROFS
      val spipe = ESPIPE
      val srch = ESRCH
      val stale = ESTALE
      val time = ETIME
      val timedout = ETIMEDOUT
      val toobig = E2BIG
      val txtbsy = ETXTBSY
      val wouldblock = EWOULDBLOCK
      val xdev = EXDEV

      local
         infixr 5 ::?
         fun (n,s) ::? l =
            if n = C_Int.fromInt ~1
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

      val toWord = SysWord.fromLargeInt o C_Int.toLarge
      val fromWord = C_Int.fromLarge o SysWord.toLargeInt

      val cleared : syserror = 0

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

      fun errorMsg (n: C_Int.t) =
         let
            val cs = strError n
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
                                 val e = getErrno ()
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
