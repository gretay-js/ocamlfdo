open Core

let verbose = ref true

let print_hot oc ~functions ~check =
  (* Function section names for ocaml functions are prefixed with .text.caml.
     but function section names for C functions have just .text. prefix,
     including C and assembly functions from ocaml runtime. We guess from the
     function symbol whether it is an ocaml or a C function, based on the
     naming conventions used by ocaml compiler code generation. This is an
     implicit dependency that might silently break if the naming convention
     changes. *)
  let ocaml_pattern =
    Re2.create_exn
      "caml[A-Z][A-Za-z0-9_]*__(entry|([a-z_][A-Za-z0-9$_]*_[0-9]*))"
  in
  let section_name n = fprintf oc "*(.text.%s)\n" n in
  let ocaml_section_name n = fprintf oc "*(.text.caml.%s)\n" n in

  (* Function symbols from ocaml are always global (so far), but function
     symbols from C can be local, for example if defined as static. Linker
     script cannot refer to local symbols, so we cannot print an assert in
     linker script for them for correct repositioning in the linker script,
     and silently fail to reorder them. At least the check works for global
     function symbols from C.

     To get a complete check, users can check using `nm -n` for example that
     these were reordered, or we can add another command that does it, e.g.
     takes as input executable and checks all symbols and their positions
     using owee. *)
  let msg =
    sprintf
      "\"Hot function %s was not placed in the hot section by the linker \
       script. Missing function section name.\n\
       \""
  in
  let cond n =
    fprintf oc
      "ASSERT (DEFINED(%s)?(caml_hot__code_begin <= %s) && (%s <= \
       caml_hot__code_end):1, \n\
       %s);\n"
      n n n (msg n)
  in
  let ocaml_cond n =
    fprintf oc
      "ASSERT ((caml_hot__code_begin <= %s) && (%s <= caml_hot__code_end), \n\
       %s);\n"
      n n (msg n)
  in
  let print_check n = function
    | true -> ocaml_cond n
    | false -> cond n
  in
  let print_section n = function
    | true -> ocaml_section_name n
    | false ->
        section_name n;
        ocaml_section_name n
  in
  let print name =
    let is_ocaml = Re2.matches ocaml_pattern name in
    print_section name is_ocaml;
    if check then print_check name is_ocaml
  in
  (* CR-soon gyorsh: It may be convenient to generate all asserts at the end
     of the hot section, instead of interleaved with layout, if users ever
     look at the layout. *)
  List.iter functions ~f:print

let write_hot filename profile ~reorder_functions ~check =
  if !verbose then printf "Writing linker script hot to %s\n" filename;
  let functions = Reorder.hot_functions profile ~reorder_functions in
  Out_channel.with_file filename ~f:(print_hot ~functions ~check)

let write ~output_filename ~linker_script_template ~linker_script_hot
    ~profile_filename ~reorder_functions ~check =
  let output_filename =
    Option.value output_filename ~default:"linker-script"
  in
  let default =
    Filename.dirname Sys.executable_name ^ "/../etc/ocamlfdo/linker-script"
  in
  let template = Option.value linker_script_template ~default in
  if !verbose then (
    printf "Writing linker script to %s\n" output_filename;
    printf "Template %s\n" template );

  let print_hot oc =
    match (linker_script_hot, profile_filename) with
    | None, None ->
        if (* just remove the marker *)
           !verbose then
          printf
            "No linker-script-hot provided, removing the marker from \
             linker-script-template.\n"
    | Some hot, None ->
        if !verbose then printf "Hot function layout %s\n" hot;
        In_channel.with_file hot
          ~f:(In_channel.iter_lines ~f:(fun s -> fprintf oc "\t%s\n" s))
    | None, Some filename ->
        if !verbose then
          printf "Hot function layout from profile %s\n" filename;
        let profile = Aggregated_decoded_profile.read_bin filename in
        let functions = Reorder.hot_functions profile ~reorder_functions in
        print_hot oc ~functions ~check
    | Some _, Some _ -> assert false
  in
  Out_channel.with_file output_filename ~f:(fun oc ->
      In_channel.with_file template
        ~f:
          (In_channel.iter_lines ~f:(fun line ->
               if
                 String.equal (String.strip line) "INCLUDE linker-script-hot"
               then print_hot oc
               else Out_channel.fprintf oc "%s\n" line)))
