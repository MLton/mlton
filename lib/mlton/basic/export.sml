structure Export: EXPORT =
struct
   
structure NJ = Pervasive.SMLofNJ

fun exportFn(file, command) =
   NJ.exportFn(File.toString file,
	       fn arg => ((command arg)
			  handle exn => (print ("Unhandled exception: "
						^ exnName exn ^ "\n") ;
					 raise exn)))
   
end
