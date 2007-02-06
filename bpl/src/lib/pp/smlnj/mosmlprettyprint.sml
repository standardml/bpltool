structure PrettyPrint = struct
   type ppstream = General.ppstream
   exception PP_FAIL of string
   open PP
end