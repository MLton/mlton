(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure PosixSysDB: POSIX_SYS_DB =
   struct
      structure CS = C.CS
      structure Prim = PosixPrimitive.SysDB
      structure Error = PosixError

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
	       fun fromC(b: bool): passwd =
		  if b
		     then {name = CS.toString(C.name()),
			   uid = C.uid(),
			   gid = C.gid(),
			   home = CS.toString(C.dir()),
			   shell = CS.toString(C.shell())}
		  else Error.error()
	    end

	    val name: passwd -> string = #name
	    val uid: passwd -> uid = #uid
	    val gid: passwd -> gid = #gid
	    val home: passwd -> string = #home
	    val shell: passwd -> string = #shell 
	 end

      val getpwnam = Passwd.fromC o Prim.getpwnam o String.nullTerm
      val getpwuid = Passwd.fromC o Prim.getpwuid
   
      structure Group =
	 struct
	    type group = {name: string,
			  gid: gid,
			  members: string list}

	    structure Group = Prim.Group

	    fun fromC(b: bool): group =
	       if b
		  then {name = CS.toString(Group.name()),
			gid = Group.gid(),
			members = C.CSS.toList(Group.mem())}
	       else Error.error()
		  
	    val name: group -> string = #name
	    val gid: group -> gid = #gid
	    val members: group -> string list = #members
	 end

      val getgrnam = Group.fromC o Prim.getgrnam o String.nullTerm
      val getgrgid = Group.fromC o Prim.getgrgid
   end
