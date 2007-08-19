(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure PosixSysDB: POSIX_SYS_DB =
   struct
      structure Prim = PrimitiveFFI.Posix.SysDB
      structure Error = PosixError
      structure SysCall = Error.SysCall

      type uid = C_UId.t
      type gid = C_GId.t

      structure Passwd =
         struct
            type passwd = {name: string,
                           uid: uid,
                           gid: gid,
                           home: string,
                           shell: string}

            structure Passwd = Prim.Passwd

            fun fromC (f: unit -> C_Int.t C_Errno.t, fname, fitem): passwd =
               SysCall.syscallErr
               ({clear = true, restart = false, errVal = C_Int.zero}, fn () =>
                {return = f (),
                 post = fn _ => {name = CUtil.C_String.toString (Passwd.getName ()),
                                 uid = Passwd.getUId (),
                                 gid = Passwd.getGId (),
                                 home = CUtil.C_String.toString (Passwd.getDir ()),
                                 shell = CUtil.C_String.toString (Passwd.getShell ())},
                 handlers = [(Error.cleared, fn () => 
                              raise Error.SysErr (concat ["Posix.SysDB.",
                                                          fname,
                                                          ": no group with ",
                                                          fitem], NONE))]})

            val name: passwd -> string = #name
            val uid: passwd -> uid = #uid
            val gid: passwd -> gid = #gid
            val home: passwd -> string = #home
            val shell: passwd -> string = #shell 
         end

      fun getpwnam name = 
         let val name = NullString.nullTerm name
         in Passwd.fromC (fn () => Prim.getpwnam name, "getpwnam", "name")
         end

      fun getpwuid uid = 
         Passwd.fromC (fn () => Prim.getpwuid uid, "getpwuid", "user id")

      structure Group =
         struct
            type group = {name: string,
                          gid: gid,
                          members: string list}

            structure Group = Prim.Group

            fun fromC (f: unit -> C_Int.t C_Errno.t, fname, fitem): group =
               SysCall.syscallErr
               ({clear = true, restart = false, errVal = C_Int.zero}, fn () =>
                {return = f (),
                 post = fn _ => {name = CUtil.C_String.toString (Group.getName ()),
                                 gid = Group.getGId (),
                                 members = CUtil.C_StringArray.toList (Group.getMem ())},
                 handlers = [(Error.cleared, fn () => 
                              raise Error.SysErr (concat ["Posix.SysDB.",
                                                          fname,
                                                          ": no group with ",
                                                          fitem], NONE))]})

            val name: group -> string = #name
            val gid: group -> gid = #gid
            val members: group -> string list = #members
         end

      fun getgrnam name = 
         let val name = NullString.nullTerm name
         in Group.fromC (fn () => Prim.getgrnam name, "getgrnam", "name")
         end

      fun getgrgid gid = 
         Group.fromC (fn () => Prim.getgrgid gid, "getgrgid", "group id")
   end
