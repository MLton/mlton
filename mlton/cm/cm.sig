signature CM =
   sig
      (* cmfile can be relative or absolute.
       * The resulting list of files will have the same path as cmfile.
       *)
      val cm: {cmfile: File.t} -> File.t list
   end
