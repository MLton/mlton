signature OS_IO =
   sig
      eqtype iodesc
      eqtype iodesc_kind

      val hash: iodesc -> word
      val compare: iodesc * iodesc -> order
      val kind: iodesc -> iodesc_kind

      structure Kind: 
         sig
            val file: iodesc_kind
            val dir: iodesc_kind
            val symlink: iodesc_kind
            val tty: iodesc_kind
            val pipe: iodesc_kind
            val socket: iodesc_kind
            val device: iodesc_kind
         end

      eqtype poll_desc
      type poll_info
      val pollDesc: iodesc -> poll_desc option
      val pollToIODesc: poll_desc -> iodesc
      exception Poll
      val pollIn: poll_desc -> poll_desc
      val pollOut: poll_desc -> poll_desc
      val pollPri: poll_desc -> poll_desc
      val poll: poll_desc list * Time.time option -> poll_info list
      val isIn: poll_info -> bool
      val isOut: poll_info -> bool
      val isPri: poll_info -> bool
      val infoToPollDesc: poll_info -> poll_desc
   end
