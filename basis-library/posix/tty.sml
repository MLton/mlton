(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PosixTTY: POSIX_TTY =
   struct
      structure Prim = PrimitiveFFI.Posix.TTY
      open Prim
      structure Error = PosixError
      structure SysCall = Error.SysCall

      type pid = C_PId.t

      type file_desc = C_Fd.t

      structure V =
         struct
            open V
            val nccs = C_Int.toInt NCCS
            val eof = C_Int.toInt VEOF
            val eol = C_Int.toInt VEOL
            val erase = C_Int.toInt VERASE
            val intr = C_Int.toInt VINTR
            val kill = C_Int.toInt VKILL
            val min = C_Int.toInt VMIN
            val quit = C_Int.toInt VQUIT
            val susp = C_Int.toInt VSUSP
            val time = C_Int.toInt VTIME
            val start = C_Int.toInt VSTART
            val stop = C_Int.toInt VSTOP

            type cc = C_CC.t array

            val default = C_CC.castFromSysWord 0w0

            fun new () = Array.array (nccs, default)

            fun updates (a, l) = 
               List.app (fn (i, cc) => 
                         Array.update (a, i, (C_CC.castFromSysWord o Word8.castToSysWord o Byte.charToByte) cc)) 
                        l

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

            val sub = (Byte.byteToChar o Word8.castFromSysWord o C_CC.castToSysWord) o Array.sub
         end

      structure Flags = BitFlags(structure S = C_TCFlag)
      structure I =
         struct
            open I Flags
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

      structure O =
         struct
            open O Flags
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

      structure C =
         struct
            open C Flags
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

      structure L =
         struct
            open L Flags
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

      type speed = C_Speed.t

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

      val compareSpeed = C_Speed.compare
      val speedToWord = C_Speed.castToSysWord
      val wordToSpeed = C_Speed.castFromSysWord

      type termios = {iflag: I.flags,
                      oflag: O.flags,
                      cflag: C.flags,
                      lflag: L.flags,
                      cc: V.cc,
                      ispeed: speed,
                      ospeed: speed}

      val id = fn x => x
      val termios = id
      val fieldsOf = id

      val getiflag: termios -> I.flags = #iflag
      val getoflag: termios -> O.flags = #oflag
      val getcflag: termios -> C.flags = #cflag
      val getlflag: termios -> L.flags = #oflag
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

            type set_action = C_Int.t
            val sadrain = TCSADRAIN
            val saflush = TCSAFLUSH
            val sanow = TCSANOW

            type flow_action = C_Int.t
            val ioff = TCIOFF
            val ion = TCION
            val ooff = TCOOFF
            val oon = TCOON

            type queue_sel = C_Int.t
            val iflush = TCIFLUSH
            val oflush = TCOFLUSH
            val ioflush = TCIOFLUSH

            fun getattr fd =
               SysCall.syscallRestart
               (fn () =>
                (Prim.TC.getattr fd, fn _ =>
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
                 ; (Prim.TC.setattr (fd, a), fn _ => ())))

            fun sendbreak (fd, n) =
               SysCall.simpleRestart (fn () => Prim.TC.sendbreak (fd, C_Int.fromInt n))

            fun drain fd = SysCall.simpleRestart (fn () => Prim.TC.drain fd)

            fun flush (fd, n) =
               SysCall.simpleRestart (fn () => Prim.TC.flush (fd, n))

            fun flow (fd, n) =
               SysCall.simpleRestart (fn () => Prim.TC.flow (fd, n))

            fun getpgrp fd =
               SysCall.simpleResultRestart'
               ({errVal = C_PId.castFromFixedInt ~1}, fn () =>
                Prim.TC.getpgrp fd)

            fun setpgrp (fd, pid) = 
               SysCall.simpleRestart (fn () => Prim.TC.setpgrp (fd, pid))
         end
   end
