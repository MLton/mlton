(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PosixTTY: POSIX_TTY =
   struct
      structure Cstring = COld.CS
      structure Prim = PrimitiveFFI.Posix.TTY
      open Prim
      structure Error = PosixError
      structure SysCall = Error.SysCall

      type pid = C.PId.t
         
      type file_desc = C.Fd.t
         
      structure V =
         struct
            open V
            val nccs = NCCS
            val eof = VEOF
            val eol = VEOL
            val erase = VERASE
            val intr = VINTR
            val kill = VKILL
            val min = VMIN
            val quit = VQUIT
            val susp = VSUSP
            val time = VTIME
            val start = VSTART
            val stop = VSTOP

            type cc = C.CC.t array

            val default = Byte.charToByte #"\000"

            fun new () = Array.array (NCCS, default)

            fun updates (a, l) = 
               List.app (fn (i, cc) => Array.update (a, i, Byte.charToByte cc)) l

            fun cc l = let val a = new ()
                       in updates (a, l)
                          ; a
                       end

            fun update (a, l) =
               let val a' = new ()
               in Array.copy {src = a, dst = a', di = 0}
                  ; updates (a', l)
                  ; a'
               end

            val sub = Byte.byteToChar o Array.sub
         end
      
      structure IFlags =
         struct
            open IFlags BitFlags
            val brkint = BRKINT
            val icrnl = ICRNL
            val ignbrk = IGNBRK
            val igncr = IGNCR
            val ignpar = IGNPAR
            val inlcr = INLCR
            val inpck = INPCK
            val istrip = ISTRIP
            val ixany = IXANY
            val ixoff = IXOFF
            val ixon = IXON
            val parmrk = PARMRK
         end
      
      structure OFlags =
         struct
            open OFlags BitFlags
            val bs0 = BS0
            val bs1 = BS1
            val bsdly = BSDLY
            val cr0 = CR0
            val cr1 = CR1
            val cr2 = CR2
            val cr3 = CR3
            val crdly = CRDLY
            val ff0 = FF0
            val ff1 = FF1
            val ffdly = FFDLY
            val nl0 = NL0
            val nl1 = NL1
            val onldly = NLDLY
            val ocrnl = OCRNL
            val ofill = OFILL
            val onlcr = ONLCR
            val onlret = ONLRET
            val onocr = ONOCR
            val opost = OPOST
            val tab0 = TAB0
            val tab1 = TAB1
            val tab2 = TAB2
            val tab3 = TAB3
            val tabdly = TABDLY
            val vt0 = VT0
            val vt1 = VT1
            val vtdly = VTDLY
         end
      
      structure CFlags =
         struct
            open CFlags BitFlags
            val clocal = CLOCAL
            val cread = CREAD
            val cs5 = CS5
            val cs6 = CS6
            val cs7 = CS7
            val cs8 = CS8
            val csize = CSIZE
            val cstopb = CSTOPB
            val hupcl = HUPCL
            val parenb = PARENB
            val parodd = PARODD
         end
      
      structure LFlags =
         struct
            open LFlags BitFlags
            val echo = ECHO
            val echoe = ECHOE
            val echok = ECHOK
            val echonl = ECHONL
            val icanon = ICANON
            val iexten = IEXTEN
            val isig = ISIG
            val noflsh = NOFLSH
            val tostop = TOSTOP
         end

      type speed = C.Speed.t

      val b0 = B0
      val b110 = B110
      val b1200 = B1200
      val b134 = B134
      val b150 = B150
      val b1800 = B1800
      val b19200 = B19200
      val b200 = B200
      val b2400 = B2400
      val b300 = B300
      val b38400 = B38400
      val b4800 = B4800
      val b50 = B50
      val b600 = B600
      val b75 = B75
      val b9600 = B9600

      val compareSpeed = SysWord.compare
      fun id x = x
      val speedToWord = id
      val wordToSpeed = id

      type termios = {iflag: IFlags.flags,
                      oflag: OFlags.flags,
                      cflag: CFlags.flags,
                      lflag: LFlags.flags,
                      cc: V.cc,
                      ispeed: speed,
                      ospeed: speed}

      val termios = id
      val fieldsOf = id

      val getiflag: termios -> IFlags.flags = #iflag
      val getoflag: termios -> OFlags.flags = #oflag
      val getcflag: termios -> CFlags.flags = #cflag
      val getlflag: termios -> LFlags.flags = #oflag
      val getcc: termios -> V.cc = #cc

      structure CF =
         struct
            val getospeed: termios -> speed = #ospeed
            fun setospeed ({iflag, oflag, cflag, lflag, cc, ispeed, ...}: termios,
                          ospeed: speed): termios =
               {iflag = iflag,
                oflag = oflag,
                cflag = cflag,
                lflag = lflag,
                cc = cc,
                ispeed = ispeed,
                ospeed = ospeed}
                
            val getispeed: termios -> speed = #ispeed
               
            fun setispeed ({iflag, oflag, cflag, lflag, cc, ospeed, ...}: termios,
                          ispeed: speed): termios =
               {iflag = iflag,
                oflag = oflag,
                cflag = cflag,
                lflag = lflag,
                cc = cc,
                ispeed = ispeed,
                ospeed = ospeed}
         end
      
      structure Termios = Prim.Termios
         
      structure TC =
         struct
            open Prim.TC 

            type set_action = C.Int.t
            val sadrain = TCSADRAIN
            val saflush = TCSAFLUSH
            val sanow = TCSANOW

            type flow_action = C.Int.t
            val ioff = TCIOFF
            val ion = TCION
            val ooff = TCOOFF
            val oon = TCOON

            type queue_sel = C.Int.t
            val iflush = TCIFLUSH
            val oflush = TCOFLUSH
            val ioflush = TCIOFLUSH

            fun getattr fd =
               SysCall.syscallRestart
               (fn () =>
                (Prim.TC.getattr fd, fn () =>
                 {iflag = Termios.getIFlag (),
                  oflag = Termios.getOFlag (),
                  cflag = Termios.getCFlag (),
                  lflag = Termios.getLFlag (),
                  cc = let val a = V.new ()
                       in Termios.getCC (a); a
                       end,
                  ispeed = Termios.cfGetISpeed (),
                  ospeed = Termios.cfGetOSpeed ()}))

            fun setattr (fd, a,
                         {iflag, oflag, cflag, lflag, cc, ispeed, ospeed}) =
               SysCall.syscallRestart
               (fn () =>
                (Termios.setIFlag iflag
                 ; Termios.setOFlag oflag
                 ; Termios.setCFlag cflag
                 ; Termios.setLFlag lflag
                 ; SysCall.simple (fn () => Termios.cfSetOSpeed ospeed)
                 ; SysCall.simple (fn () => Termios.cfSetISpeed ispeed)
                 ; Termios.setCC cc
                 ; (Prim.TC.setattr (fd, a), fn () => ())))

            fun sendbreak (fd, n) =
               SysCall.simpleRestart (fn () => Prim.TC.sendbreak (fd, n))

            fun drain fd = SysCall.simpleRestart (fn () => Prim.TC.drain fd)
              
            fun flush (fd, n) =
               SysCall.simpleRestart (fn () => Prim.TC.flush (fd, n))
              
            fun flow (fd, n) =
               SysCall.simpleRestart (fn () => Prim.TC.flow (fd, n))
              
            fun getpgrp fd =
               SysCall.syscallRestart
               (fn () =>
                let val pid = Prim.TC.getpgrp fd
                in (Pid.toInt pid, fn () => pid)
                end)
              
            fun setpgrp (fd, pid) = 
               SysCall.simpleRestart (fn () => Prim.TC.setpgrp (fd, pid))
         end

      structure C = CFlags
      structure I = IFlags
      structure L = LFlags
      structure O = OFlags
   end
