include module type of struct
  include Map_with_default_intf
end

module Make (Key: Key) : S with type Key.t = Key.t
