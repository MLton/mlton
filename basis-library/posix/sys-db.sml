(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure PosixSysDB: POSIX_SYS_DB =
   struct
      structure CS = C.CS
      structure Prim = PosixPrimitive.SysDB
      structure Error = PosixError
      structure SysCall = Error.SysCall

      val stub = Error.stubMinGW

      type uid = Prim.uid
      type gid = Prim.gid

      structure Passwd =
	 struct
	    type passwd = {name: string,
			   uid: uid,
			   gid: gid,
			   home: string,
			   shell: string}

	    local
	       structure C = Prim.Passwd
	    in
	       fun fromC (f: unit -> bool): passwd =
		  SysCall.syscall
		  (fn () =>
		   (if f () then 0 else ~1,
		    fn () => {name = CS.toString(C.name()),
			      uid = C.uid(),
			      gid = C.gid(),
			      home = CS.toString(C.dir()),
			      shell = CS.toString(C.shell())}))
	    end

	    val name: passwd -> string = #name
	    val uid: passwd -> uid = #uid
	    val gid: passwd -> gid = #gid
	    val home: passwd -> string = #home
	    val shell: passwd -> string = #shell 
	 end

      fun getpwnam name = 
	 let val name = NullString.nullTerm name
	 in Passwd.fromC (fn () => Prim.getpwnam name)
	 end

      fun getpwuid uid = Passwd.fromC (fn () => Prim.getpwuid uid)

      structure Group =
	 struct
	    type group = {name: string,
			  gid: gid,
			  members: string list}

	    structure Group = Prim.Group

	    fun fromC (f: unit -> bool): group =
	       SysCall.syscall
	       (fn () =>
		(if f () then 0 else ~1,
		 fn () => {name = CS.toString(Group.name()),
			   gid = Group.gid(),
			   members = C.CSS.toList(Group.mem())}))
		  
	    val name: group -> string = #name
	    val gid: group -> gid = #gid
	    val members: group -> string list = #members
	 end

      fun getgrnam name = 
	 let val name = NullString.nullTerm name
	 in Group.fromC (fn () => Prim.getgrnam name)
	 end
      
      fun getgrgid gid = Group.fromC (fn () => Prim.getgrgid gid)

      val getgrgid = stub getgrgid
      val getgrnam = stub getgrnam
      val getpwuid = stub getpwuid
   end
