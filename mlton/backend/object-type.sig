signature OBJECT_TYPE =
   sig
      structure PointerTycon: POINTER_TYCON
      structure Runtime: RUNTIME
	 
      type ty
      datatype t =
	 Array of ty
       | Normal of ty
       | Stack
       | Weak of ty (* in Weak t, must have Type.isPointer t *)
       | WeakGone
	 
      val basic: (PointerTycon.t * t) vector
      val isOk: t -> bool
      val layout: t -> Layout.t
      val toRuntime: t -> Runtime.RObjectType.t
   end
