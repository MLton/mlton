signature T =   
   sig
      eqtype s
      structure V:
         sig
            datatype v = V
         end where type v = s
   end 

signature S =
   sig
      eqtype v
      structure T: sig
                      eqtype t
                   end where type t = v
   end

signature S =
   sig
      eqtype v
      structure S: sig
                      type 'a t
                   end where type 'a t = v
      structure T: sig
                      eqtype t
                   end where type t = int S.t
   end

signature S =
   sig
      eqtype v
      structure S: sig
                      type 'a t
                   end where type 'a t = v
      structure T: sig
                      eqtype t
                   end where type t = real S.t
   end

signature S =
   sig
      eqtype v
      structure S: sig
                      type 'a t
                      type u = real t
                   end where type 'a t = v
      structure T: sig
                      eqtype t
                   end where type t = S.u
   end

functor F (eqtype v
           structure S: sig
                           type 'a t
                           type u = real t
                        end where type 'a t = v) =
   struct
      fun f (x: 'a S.t) = x = x
      fun f (x: S.u) = x = x
   end

signature T =   
   sig
      eqtype s
      structure U:
         sig
            type 'a t
            type u = (int * real) t
         end where type 'a t = s
      structure V:
         sig
            datatype v = V
         end where type v = U.u
   end

structure T: T =
   struct
      datatype s = V
      structure U =
         struct
            type 'a t = s
            type u = (int * real) t
         end
      structure V =
         struct
            datatype v = datatype s
         end
   end
