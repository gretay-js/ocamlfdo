module type S = sig
  module Func_data : sig type t end
  module Block_data : sig type t end
  module Instr_data : sig type t end
end
