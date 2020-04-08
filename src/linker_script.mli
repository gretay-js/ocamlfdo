val write_hot :
  string ->
  Aggregated_decoded_profile.t ->
  reorder_functions:Config_reorder.Reorder_functions.t ->
  cutoff:Config_reorder.Cutoff_functions.t ->
  check:bool ->
  unit

val write :
  output_filename:string option ->
  linker_script_template:string option ->
  linker_script_hot:string option ->
  profile_filename:string option ->
  reorder_functions:Config_reorder.Reorder_functions.t ->
  cutoff:Config_reorder.Cutoff_functions.t ->
  check:bool ->
  unit

val check_function_order :
  binary_filename:string ->
  profile_filename:string ->
  reorder_functions:Config_reorder.Reorder_functions.t ->
  cutoff:Config_reorder.Cutoff_functions.t ->
  output_filename:string option ->
  unit

val randomize_function_order :
  binary_filename:string ->
  output_filename:string option ->
  check:bool ->
  unit

val verbose : bool ref
