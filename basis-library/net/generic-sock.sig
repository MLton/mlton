signature GENERIC_SOCK =
   sig
      val socket: Socket.AF.addr_family * Socket.SOCK.sock_type -> 
                  ('af, 'sock_type) Socket.sock
      val socketPair: Socket.AF.addr_family * Socket.SOCK.sock_type -> 
                      ('af, 'sock_type) Socket.sock * ('af, 'sock_type) Socket.sock
      val socket': Socket.AF.addr_family * Socket.SOCK.sock_type * int -> 
                   ('af, 'sock_type) Socket.sock
      val socketPair': Socket.AF.addr_family * Socket.SOCK.sock_type * int -> 
                       ('af, 'sock_type) Socket.sock * ('af, 'sock_type) Socket.sock
   end
