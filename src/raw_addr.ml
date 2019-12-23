module T = struct
  include Core.Int64
end

include T
include Core.Hashable.Make_binable (T)
