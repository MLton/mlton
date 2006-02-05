signature SML_OF_NJ =
   sig
      structure Cont:
         sig
            type 'a cont
            val callcc: ('a cont -> 'a) -> 'a
            val throw: 'a cont -> 'a -> 'b
         end
      structure SysInfo:
         sig
            exception UNKNOWN
            datatype os_kind = BEOS | MACOS | OS2 | UNIX | WIN32

            val getHostArch: unit -> string
            val getOSKind: unit -> os_kind
            val getOSName: unit -> string
         end

      val exnHistory: exn -> string list
      val exportFn: string * (string * string list -> OS.Process.status) -> unit
      val exportML: string -> bool
      val getAllArgs: unit -> string list
      val getArgs: unit -> string list
      val getCmdName: unit -> string
   end
