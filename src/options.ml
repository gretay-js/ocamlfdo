type reorder_basic_blocks =  None
type reorder_functions = None | Exec_counts

type t = {
  gen_linearid_profile : string;
  write_bolt_fdata : bool;
  write_linker_script : bool;
  reorder_basic_blocks : reorder_basic_blocks;
  reorder_functions : reorder_functions;
}

let mk gen_linearid_profile = {
  gen_linearid_profile;
  write_bolt_fdata = true;
  write_linker_script = true;
  reorder_functions = None;
  reorder_basic_blocks = None;
}
