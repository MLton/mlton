(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
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

      val errorNames =
         [
          (acces,"acces"),
          (addrinuse,"addrinuse"),
          (addrnotavail,"addrnotavail"),
          (afnosupport,"afnosupport"),
          (again,"again"),
          (already,"already"),
          (badf,"badf"),
          (badmsg,"badmsg"),
          (busy,"busy"),
          (canceled,"canceled"),
          (child,"child"),
          (connaborted,"connaborted"),
          (connrefused,"connrefused"),
          (connreset,"connreset"),
          (deadlk,"deadlk"),
          (destaddrreq,"destaddrreq"),
          (dom,"dom"),
          (dquot,"dquot"),
          (exist,"exist"),
          (fault,"fault"),
          (fbig,"fbig"),
          (hostunreach,"hostunreach"),
          (idrm,"idrm"),
          (ilseq,"ilseq"),
          (inprogress,"inprogress"),
          (intr,"intr"),
          (inval,"inval"),
          (io,"io"),
          (isconn,"isconn"),
          (isdir,"isdir"),
          (loop,"loop"),
          (mfile,"mfile"),
          (mlink,"mlink"),
          (msgsize,"msgsize"),
          (multihop,"multihop"),
          (nametoolong,"nametoolong"),
          (netdown,"netdown"),
          (netreset,"netreset"),
          (netunreach,"netunreach"),
          (nfile,"nfile"),
          (nobufs,"nobufs"),
          (nodata,"nodata"),
          (nodev,"nodev"),
          (noent,"noent"),
          (noexec,"noexec"),
          (nolck,"nolck"),
          (nolink,"nolink"),
          (nomem,"nomem"),
          (nomsg,"nomsg"),
          (noprotoopt,"noprotoopt"),
          (nospc,"nospc"),
          (nosr,"nosr"),
          (nostr,"nostr"),
          (nosys,"nosys"),
          (notconn,"notconn"),
          (notdir,"notdir"),
          (notempty,"notempty"),
          (notsock,"notsock"),
          (notsup,"notsup"),
          (notty,"notty"),
          (nxio,"nxio"),
          (opnotsupp,"opnotsupp"),
          (overflow,"overflow"),
          (perm,"perm"),
          (pipe,"pipe"),
          (proto,"proto"),
          (protonosupport,"protonosupport"),
          (prototype,"prototype"),
          (range,"range"),
          (rofs,"rofs"),
          (spipe,"spipe"),
          (srch,"srch"),
          (stale,"stale"),
          (time,"time"),
          (timedout,"timedout"),
          (toobig,"toobig"),
          (txtbsy,"txtbsy"),
          (wouldblock,"wouldblock"),
          (xdev,"xdev")
         ]

      exception SysErr of string * syserror option

      val toWord = SysWord.fromInt
      val fromWord = SysWord.toInt

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

      fun errorMsg (n: int) =
         let
            val cs = strError n
         in
            if cs = Primitive.Pointer.null
               then "Unknown error"
            else COld.CS.toString cs
         end

      fun raiseSys n = raise SysErr (errorMsg n, SOME n)

      structure SysCall =
         struct
            structure Thread = Primitive.Thread

            val blocker: (unit -> (unit -> unit)) ref =
               ref (fn () => (fn () => ()))
               (* ref (fn () => raise Fail "blocker not installed") *)
            val restartFlag = ref true

            val syscallErr: {clear: bool, restart: bool} * 
                            (unit -> {return: int,
                                      post: unit -> 'a,
                                      handlers: (syserror * (unit -> 'a)) list}) -> 'a =
               fn ({clear, restart}, f) =>
               let
                  fun call (err: {errno: syserror,
                                  handlers: (syserror * (unit -> 'a)) list} -> 'a): 'a =
                     let
                        val () = Thread.atomicBegin ()
                        val () = if clear then clearErrno () else ()
                        val {return, post, handlers} = 
                           f () handle exn => (Thread.atomicEnd (); raise exn)
                     in
                        if ~1 = return
                           then
                              (* Must getErrno () in the critical section. *)
                              let
                                 val e = getErrno ()
                                 val () = Thread.atomicEnd ()
                              in
                                 err {errno = e, handlers = handlers}
                              end
                           else DynamicWind.wind (post, Thread.atomicEnd)
                     end
                  fun err {default: unit -> 'a, 
                           errno: syserror, 
                           handlers: (syserror * (unit -> 'a)) list}: 'a =
                     case List.find (fn (e',_) => errno = e') handlers of
                        NONE => default ()
                      | SOME (_, handler) => handler ()
                  fun errBlocked {errno: syserror,
                                  handlers: (syserror * (unit -> 'a)) list}: 'a =
                     err {default = fn () => raiseSys errno,
                          errno = errno, handlers = handlers}
                  fun errUnblocked
                     {errno: syserror,
                      handlers: (syserror * (unit -> 'a)) list}: 'a =
                     err {default = fn () =>
                          if restart andalso errno = intr andalso !restartFlag
                             then if Thread.canHandle () = 0
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
               val simpleResult' = fn ({restart}, f) =>
                  syscallErr 
                  ({clear = false, restart = restart}, fn () => 
                   let val return = f () 
                   in {return = return, post = fn () => return, handlers = []}
                   end)
            in
               val simpleResultRestart = fn f =>
                  simpleResult' ({restart = true}, f)
               val simpleResult = fn f =>
                  simpleResult' ({restart = false}, f)
            end
         
            val simpleRestart = ignore o simpleResultRestart
            val simple = ignore o simpleResult

            val syscallRestart = fn f => 
               syscallErr 
               ({clear = false, restart = true}, fn () => 
                let val (return, post) = f () 
                in {return = return, post = post, handlers = []}
                end)
            val syscall = fn f =>
               syscallErr 
               ({clear = false, restart = false}, fn () => 
                let val (return, post) = f () 
                in {return = return, post = post, handlers = []}
                end)
         end
   end
