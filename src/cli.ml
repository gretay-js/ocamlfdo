open Core
open Main_fdo

let set_verbose v =
  verbose := v;
  Perf_profile.verbose := v;
  Aggregated_decoded_profile.verbose := v;
  Aggregated_perf_profile.verbose := v;
  Bolt_profile.verbose := v;
  Decoded_bolt_profile.verbose := v;
  Linearid_profile.verbose := v;
  Cfg_info.verbose := v;
  Clusters.verbose := v;
  Elf_locations.verbose := v;
  Filenames.verbose := v;
  Reorder.verbose := v;
  Report.verbose := v;
  Ocamlcfg.Util.verbose := v;
  Crcs.verbose := v;
  Wrapper.verbose := v;
  Linker_script.verbose := v;
  Merge.verbose := v;
  ()

(* Utility for handling variant type command line options. *)
(* Print all variants in option's help string. *)
module type Alt = sig
  type t

  val to_string : t -> string

  val all : t list

  val default : t
end

module AltFlag (M : Alt) = struct
  let of_string s =
    match List.find M.all ~f:(fun t -> String.equal s (M.to_string t)) with
    | None -> failwith ("unknown option argument" ^ s)
    | Some t -> t

  let alternatives heading =
    let names = List.map M.all ~f:M.to_string in
    let default = M.(to_string default) in
    sprintf "%s: %s (default: %s)" heading
      (String.concat ~sep:"," names)
      default

  let mk name ~doc =
    assert (not (List.contains_dup ~compare:Poly.compare M.all));
    Command.Param.(
      flag name
        (optional_with_default M.default (Command.Arg_type.create of_string))
        ~doc:(alternatives doc))
end

module Commonflag = struct
  type t =
    { name : string;
      doc : string;
      aliases : string list
    }

  let optional t =
    Command.Param.(
      flag ~aliases:t.aliases t.name (optional Filename.arg_type) ~doc:t.doc)

  let required t =
    Command.Param.(
      flag ~aliases:t.aliases t.name (required Filename.arg_type) ~doc:t.doc)

  let flag_binary_filename =
    { name = "-binary";
      doc = "filename elf binary to optimize";
      aliases = []
    }

  let flag_perf_profile_filename =
    { name = "-perf-profile";
      doc = "perf.data output of perf record";
      aliases = []
    }

  let flag_profile_filename =
    { name = "-fdo-profile"; doc = "filename decoded profile"; aliases = [] }

  let flag_output_filename =
    { name = "-o"; doc = "filename output"; aliases = [] }

  let flag_linker_script_template_filename =
    { name = "-linker-script-template";
      doc = "filename linker script template";
      aliases = []
    }

  let flag_linker_script_hot_filename =
    { name = "-linker-script-hot";
      doc = "filename hot functions layout for linker script";
      aliases = []
    }
end

let flag_report =
  Command.Param.(
    flag "-fdo-report" no_arg
      ~doc:
        " emit .fdo.org files showing FDO decisions (e.g., blocks reordered)")

let flag_dot =
  Command.Param.(
    flag "-dot" no_arg ~doc:" emit CFG in .dot format for debug")

let flag_dot_show_instr =
  Command.Param.(
    flag "-dot-detailed" no_arg
      ~doc:" emit detailed CFG in .dot format for debug")

let flag_v =
  Command.Param.(
    flag "-verbose" ~aliases:["-v"] no_arg
      ~doc:" print lots of info for debug")

let flag_q =
  Command.Param.(
    flag "-quiet" ~aliases:["-q"] no_arg ~doc:" don't print anything")

let flag_expected_pids =
  Command.Param.(
    flag "-pids"
      (optional_with_default []
         (Arg_type.comma_separated ~allow_empty:false ~strip_whitespace:true
            ~unique_values:true int))
      ~doc:
        "pids include samples only from these pids, specified as a \
         comma-separated list of integers")

let flag_seed =
  Command.Param.(
    flag "-seed" (optional int)
      ~doc:"int seed for some transformation that use random")

let flag_ignore_buildid =
  Command.Param.(
    flag "-ignore-buildid" no_arg
      ~doc:" ignore mismatch in buildid between binary and perf.data")

let flag_force =
  Command.Param.(flag "-f" no_arg ~doc:" no assertions in linker-script-hot")

let flag_write_linker_script_hot =
  Command.Param.(
    flag "-write-linker-script-hot" no_arg
      ~doc:" write hot functions layout for linker script to a file")

let flag_write_aggregated_profile =
  Command.Param.(
    flag "-write-aggregated" no_arg
      ~doc:" write counters aggregated from perf profile (not decoded)")

let flag_read_aggregated_perf_profile =
  Command.Param.(
    flag "-read-aggregated" no_arg
      ~doc:" read aggregated counters (not decoded)")

let flag_extra_debug =
  Command.Param.(
    flag "-extra-debug" no_arg
      ~doc:
        " add extra debug info to generated code to enable profile decoding")

let flag_auto =
  Command.Param.(
    flag "-auto" no_arg
      ~doc:
        " Automatically figure out how to build.\n\
        \         Given -fdo-profile <file>, if <file> does not exist, then \
         add -extra-debug. Without -fdo-profile, invoke ocamlopt without \
         splitting it into phases (ignore phase-specific ocamlfdo \
         arguments).")

let flag_get_config =
  let open Command.Param in
  (* CR-someday gyorsh: this is just boolean combination of func and unit. Is
     there a cleaner way to define it as cmdline options? *)
  let flag_no_crc =
    flag "-no-md5" no_arg ~doc:" do not generate any -md5."
    |> map ~f:(function
         | true -> Some (Crcs.Config.mk ~func:false ~unit:false)
         | false -> None)
  in
  let flag_all_crc =
    let open Command.Param in
    flag "-md5" no_arg
      ~doc:
        " use md5 to detect source changes at function and compilation unit \
         level (implies both -md5-unit and -md5-fun)"
    |> map ~f:(function
         | true -> Some (Crcs.Config.mk ~func:true ~unit:true)
         | false -> None)
  in
  let flag_unit_crc =
    let open Command.Param in
    flag "-md5-unit" no_arg
      ~doc:" use md5 per compilation unit only to detect source changes"
    |> map ~f:(function
         | true -> Some (Crcs.Config.mk ~func:false ~unit:true)
         | false -> None)
  in
  let flag_func_crc =
    let open Command.Param in
    flag "-md5-fun" no_arg
      ~doc:" use md5 per function only to detect source changes"
    |> map ~f:(function
         | true -> Some (Crcs.Config.mk ~func:true ~unit:false)
         | false -> None)
  in
  choose_one
    [flag_unit_crc; flag_func_crc; flag_all_crc; flag_no_crc]
    ~if_nothing_chosen:(Default_to (Crcs.Config.mk ~func:false ~unit:true))

let flag_crc_config =
  Command.Let_syntax.(
    let%map config = flag_get_config
    and on_mismatch =
      let module RB = AltFlag (Crcs.On_error) in
      RB.mk "-on-md5-mismatch" ~doc:"action taken when md5 mismatch occurs"
    and on_missing =
      let module RB = AltFlag (Crcs.On_error) in
      RB.mk "-on-md5-missing" ~doc:"action taken when md5 is missing "
    and ignore_dbg =
      Command.Param.(
        flag "-md5-ignore-debug"
          (optional_with_default true bool)
          ~doc:"bool ignore debug info when creating md5")
    in
    config ~on_mismatch ~on_missing ~ignore_dbg)

let flag_timings =
  Command.Param.(
    flag "-timings" no_arg ~doc:" print timings information for each pass")

let anon_files =
  Command.Param.(
    anon (non_empty_sequence_as_list ("FILENAME" %: Filename.arg_type)))

let anon_file = Command.Param.(anon ("FILENAME" %: Filename.arg_type))

let flag_reorder_blocks =
  let module RB = AltFlag (Config_reorder.Reorder_blocks) in
  RB.mk "-reorder-blocks"
    ~doc:"heuristics for reordering basic blocks of a function"

let flag_reorder_functions =
  let module RF = AltFlag (Config_reorder.Reorder_functions) in
  RF.mk "-reorder-functions"
    ~doc:"heuristics used for function layout generated in linker script"

let merge_command =
  Command.basic ~summary:"Merge profiles "
    ~readme:(fun () ->
      {| Merge decoded profiles produced by executing the same binary,
         possibly with different inputs.

         Merge aggregated (not decoded) profiles produced from the same
         executable (using buildid to identify the match).

         All input files must be of the same kind, i.e., all decoded
         or all aggregated.

         Uses buildid and MD5 digest of compilation units or functions
         to assert that profile match (i.e., come from the same executable).

         If one of the profiles is missing extra debug info on some of the
         functions, then the debug info can be copied from another profile.

         Experimental: merge decoded profiles that come from different
         versions of the executable, if the compilation unit
         or function haven't changed. For changed ones, keep both versions,
         such that the correct version will be chosen used based on the digests.
         This can be disabled and profiles merged based on unit or function names.
      |})
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and crc_config = flag_crc_config
      and ignore_buildid = flag_ignore_buildid
      and files = anon_files
      and output_filename = Commonflag.(required flag_output_filename)
      and read_aggregated_perf_profile = flag_read_aggregated_perf_profile in
      if v then set_verbose true;
      if q then set_verbose false;
      fun () ->
        merge files ~read_aggregated_perf_profile ~crc_config ~ignore_buildid
          ~output_filename)

let decode_command =
  Command.basic
    ~summary:"Decode perf.data obtained from running the executable."
    ~readme:(fun () ->
      "\n\
       Workflow:\n\
       Build your executable with ocamlfdo to enable extra debug info\n\
       for low-level optimizations (currently, only linearize pass).See \
       other subcommands for details.\n\
       Use Linux Perf to sample hardware execution counters using LBR:\n\
       $ perf record -e cycles:u -j any,u -o perf.data <prog.exe> <args..>\n\n\
       Decode the samples:\n\
       $ ocamlfdo decode -perf-profile <perf.data> -binary <prog.exe> \n\n\
       It will generate a profile in prog.exe.fdo-profile.\n\
       The profile can be used to reoptimize the executable.\n\
       With -write-linker-script-hot, ocamlfdo decode will also produce hot \
       function layout in prog.exe.linker-script-hot file.\n")
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and seed = flag_seed
      and binary_filename = Commonflag.(required flag_binary_filename)
      and perf_profile_filename =
        Commonflag.(required flag_perf_profile_filename)
      and reorder_functions = flag_reorder_functions
      and output_filename = Commonflag.(optional flag_output_filename)
      and linker_script_hot_filename =
        Commonflag.(optional flag_linker_script_hot_filename)
      and write_linker_script_hot = flag_write_linker_script_hot
      and write_aggregated_profile = flag_write_aggregated_profile
      and read_aggregated_perf_profile = flag_read_aggregated_perf_profile
      and ignore_buildid = flag_ignore_buildid
      and expected_pids = flag_expected_pids
      and crc_config = flag_crc_config
      and force = flag_force
      and timings = flag_timings in
      if v then set_verbose true;
      if q then set_verbose false;
      make_random_state seed;
      if !verbose then (
        if write_aggregated_profile && read_aggregated_perf_profile then
          printf
            "Ignoring -write-agreggated. Incompatible with -read-aggregated.\n";
        if not write_linker_script_hot then
          printf
            "Ignoring -reorder-functions when -write-linker-script-hot is \
             not provided. Call 'ocamlfdo linker-script' with fdo-profile \
             to reorder.\n" );
      fun () ->
        Profile.record_call "decode" (fun () ->
            decode ~binary_filename ~perf_profile_filename ~reorder_functions
              ~linker_script_hot_filename ~output_filename
              ~write_linker_script_hot ~ignore_buildid ~expected_pids
              ~check:(not force) ~write_aggregated_profile
              ~read_aggregated_perf_profile ~crc_config);
        if timings then
          Profile.print Format.std_formatter Profile.all_columns)

let opt_command =
  Command.basic
    ~summary:
      "Use a profile to optimize intermediate representation of the program."
    ~readme:(fun () ->
      "\n\
       For example:\n\
       $ ocamlfdo opt -fdo-profile myexe.fdo-profile foo.cmir-linear \
       bar.cmir-linear\n\
       reads a profile from myexe.fdo-profile file, uses it\n\
       to optimize foo.cmir-linear and bar.cmir-linear and save the result to\n\
       foo.cmir-linear-fdo and bar.cmir-linear-fdo.\n\
       The intermediate representation .cmir-linear files can be obtained by\n\
       $ ocamlopt -save-ir-after linearize foo.ml bar.ml <other options>\n\
       The decoded profile can be obtained by running \"ocamlfdo decode\".")
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and seed = flag_seed
      and extra_debug = flag_extra_debug
      and fdo_profile = Commonflag.(optional flag_profile_filename)
      and reorder_blocks = flag_reorder_blocks
      and report = flag_report
      and crc_config = flag_crc_config
      and files = anon_files
      and timings = flag_timings in
      if v then set_verbose true;
      if q then set_verbose false;
      make_random_state seed;
      fun () ->
        Profile.record_call "opt" (fun () ->
            optimize files ~fdo_profile ~reorder_blocks ~extra_debug
              ~crc_config ~report);
        if timings then
          Profile.print Format.std_formatter Profile.all_columns)

let check_linear2cfg_command =
  Command.basic
    ~summary:
      "Check that the transformation from Linear IR to CFG and back is \
       identity."
    ~readme:(fun () ->
      {|
        If there is any difference in the function,
        dump Linear IR before and after the transformation to files.
        There can be a differences due to dead code, even though
        dead code elimination is not performed during this check.
|})
    Command.Let_syntax.(
      let%map v = flag_v and q = flag_q and files = anon_files in
      if v then set_verbose true;
      if q then set_verbose false;
      fun () -> check files)

let compile_command =
  Command.basic ~summary:"ocamlfdo wrapper to ocamlopt"
    ~readme:(fun () ->
      {|
       For example:
       $ ocamlfdo compile -fdo-profile myexe.fdo-profile -- \
               <standard ocamlopt options including -o myexe>
       is the same as the following 3 steps:

       (1) invoke ocamlopt with "-save-ir-after linearize -stop-after linearize -g"
           in addition to the options specified after '--'.

       (2) invoke 'ocamlfdo opt -fdo-profile myexe.fdo-profile'
           with the list of intermediate representation files .cmir-linear
           produced in step (1).

       (3) invoke 'ocamlopt -start-from emit' .cmir-linear-fdo files
           produced in step (2), instead of the source files specified
           after '--' and other options are passed unchanged.

       All options provided to 'ocamlfdo compile' before and after
       '--' are passed to 'ocamlfdo opt' and 'ocamlopt' respectively,
       except file names which are adjusted according to the step.

       This command turns ocamlfdo into a wrapper of ocamlopt that can be
       used as a drop-in replacement. It allows users to run ocamlfdo
       directly, without the need to modify their build process.
       The downside is that optimizing builds redundantly repeat compilation.

       For linking, it assumes that the user invokes ocamlopt with
       '-function sections -ccopt "-Xlinker --script=linker-script"'
       The linker-script can be produced by 'ocamlfdo decode' and 'ocaml linker-script'.

       Limitations: linker-script-hot produced by 'ocamlfdo decode' is
       specific to Linux/GNU ld. Using the linker-script for function reordering
       on a different system requires ocamlopt support for named function sections,
       which is not available currently on macos and windows).
       |})
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and seed = flag_seed
      and extra_debug = flag_extra_debug
      and auto = flag_auto
      and fdo_profile = Commonflag.(optional flag_profile_filename)
      and reorder_blocks = flag_reorder_blocks
      and report = flag_report
      and crc_config = flag_crc_config
      and args =
        Command.Param.(
          flag "--" escape
            ~doc:"ocamlopt_args standard options passed to ocamlopt")
      and timings = flag_timings in
      if v then set_verbose true;
      if q then set_verbose false;
      make_random_state seed;
      let fdo =
        if auto then
          match fdo_profile with
          | None ->
              if !verbose then
                printf
                  "Missing -fdo-profile <file>, required for compilation\n\
                   when -auto is used. Calling ocamlopt directly, without\n\
                  \ splitting compilation into phases, and not intermediate \
                   IR is saved. All phase-specific arguments are ignored.\n";
              None
          | Some file ->
              if Sys.file_exists_exn file then (
                if !verbose then
                  printf
                    "With -auto, detected that -fdo-profile <%s> file does \
                     not exist.\n\n\
                    \ Setting -extra-debug to true." file;
                Some (None, true) )
              else (
                if !verbose then
                  printf "With -auto, the file -fdo-profile <%s> exists."
                    file;
                Some (fdo_profile, extra_debug) )
        else
          (* if the file doesn't exist, optimize will fail with an error. *)
          Some (fdo_profile, extra_debug)
      in
      fun () ->
        match fdo with
        | None -> ocamlopt args
        | Some (fdo_profile, extra_debug) ->
            Profile.record_call "compile" (fun () ->
                compile args ~fdo_profile ~reorder_blocks ~extra_debug
                  ~crc_config ~report;
                if timings then
                  Profile.print Format.std_formatter Profile.all_columns))

let check_function_order_command =
  Command.basic ~summary:"Check function reordering"
    ~readme:(fun () ->
      {| Given a binary and an fdo profile, check that the layout
         of hot functions in the given binary matches the profile.

      This is useful when the source code may have changed since the profile
      was created, or to ensure that function sections were enabled,
      especially when linking with external libraries or C stubs.

      Using 'ocamlfdo linker-script' we can generate a linker script
      with assertions that fail at link time if functions symbols aren't found
      in the hot segment as expected. These assertion are not very expressive.
      For example, they cannot be used to check the exact order of function
      symbols and find local function symbols. Assertions may not
      be available on other platforms and with other template linker scripts.

      It is possible to generate linker script without assertions,
      and then manually check function order on the resulting binary using 'nm -n'
      and hot functions layout that can be generated by 'ocamlfdo linker-script'.
      The command 'ocamlfdo check hot-functions-layout' automates it and
      gives a more friendly output.
      |})
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and seed = flag_seed
      and profile_filename = Commonflag.(required flag_profile_filename)
      and reorder_functions = flag_reorder_functions
      and binary_filename = Commonflag.(required flag_binary_filename)
      and output_filename = Commonflag.(optional flag_output_filename) in
      if v then set_verbose true;
      if q then set_verbose false;
      make_random_state seed;
      fun () ->
        Linker_script.check_function_order ~binary_filename ~profile_filename
          ~reorder_functions ~output_filename)

let randomize_function_order_command =
  Command.basic ~summary:"Randomize layout of all functions"
    ~readme:(fun () ->
      {| Generate a linker script fragment with random layout of all
        functions from the binary. The output can be used with
        'ocamlfdo linker-script' as an argument to -linker-script-hot option.
      |})
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and seed = flag_seed
      and binary_filename = Commonflag.(required flag_binary_filename)
      and output_filename = Commonflag.(optional flag_output_filename)
      and force = flag_force in
      if v then set_verbose true;
      if q then set_verbose false;
      make_random_state seed;
      fun () ->
        Linker_script.randomize_function_order ~binary_filename
          ~output_filename ~check:(not force))

let linker_script_command =
  Command.basic
    ~summary:
      "Create linker script from a template and a layout of hot functions."
    ~readme:(fun () ->
      {| Inserts the hot functions from linker-script-hot into the template
         linker script, replacing the marker:
         INCLUDE linker-script-hot

        A linker script can be obtained by running the command
        "ld --verbose" and patching the output. See the default template,
       distributed with the tool.

       Use "ocamlfdo decode" to generate hot function layout.
       This command performs a trivial transformation on the files.
       It is useful when the linker runs from a different directory than
       the one where the linker-script-hot file resides, such as when
       ocamlfdo is used in a build system.

       At most one of -fdo-profile and -linker-script-hot can be provided.
       If -fdo-profile is provided, hot functions layout is computed from
       the profile using a strategy specified by -reorder-functions, with default
       strategy being in the order of function execution counts.
       If -linker-script-hot is provided, function layout is read from that
       file. Without -linker-script-hot and -fdo-profile arguments, the marker is
       simply removed.

       Argument of -linker-script-hot option must be a file that contains
       a valid fragment of linker script syntax. It can be empty. If
       section names listed in the file do not exist during linking,
       link will fail with an error.
       It can happen when recompiling after source code change,
       the numbers at the end of the function symbols can change.
       Pass "-f" to force link (i.e., do not generate checks in the linker script).
       No checks are generated for C code, because local symbols can't be checked.
       Check manually with "nm -n". |})
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and seed = flag_seed
      and output_filename = Commonflag.(optional flag_output_filename)
      and linker_script_template =
        Commonflag.(optional flag_linker_script_template_filename)
      and linker_script_hot =
        Commonflag.(optional flag_linker_script_hot_filename)
      and profile_filename = Commonflag.(optional flag_profile_filename)
      and reorder_functions = flag_reorder_functions
      and force = flag_force in
      if v then set_verbose true;
      if q then set_verbose false;
      make_random_state seed;
      if Option.is_some profile_filename && Option.is_some linker_script_hot
      then
        Report.user_error
          "Please provide at most one of -fdo-profile and -linker-script-hot";
      if !verbose && Option.is_some linker_script_hot then
        printf
          "Ignoring -reorder-functions when -linker-script-hot is provided.\n";
      fun () ->
        Profile.record_call "linker_script" (fun () ->
            Linker_script.write ~output_filename ~linker_script_template
              ~linker_script_hot ~profile_filename ~reorder_functions
              ~check:(not force)))

let to_sexp_command =
  Command.basic ~summary:"Print decoded profile as sexp to stdout."
    Command.Let_syntax.(
      let%map v = flag_v and q = flag_q and input_filename = anon_file in
      if v then set_verbose true;
      if q then set_verbose false;
      fun () -> Aggregated_decoded_profile.to_sexp input_filename)

let of_sexp_command =
  Command.basic
    ~summary:"Read sexp of decoded profile and save in binary format."
    ~readme:(fun () ->
      "Read decoded profile given as sexp in the input file.\n\
       Write the profile in binary format using bin_prot to the output file.\n\
      \      ")
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and input_filename = anon_file
      and output_filename = Commonflag.(required flag_output_filename) in
      if v then set_verbose true;
      if q then set_verbose false;
      fun () ->
        Aggregated_decoded_profile.of_sexp ~input_filename ~output_filename)

let check_command =
  Command.group ~summary:"Validation utilities."
    [ ("linear2cfg", check_linear2cfg_command);
      ("hot-functions-layout", check_function_order_command) ]

let profile_command =
  Command.group ~summary:"Utilities for manipulating decoded profiles."
    [ ("to-sexp", to_sexp_command);
      ("of-sexp", of_sexp_command);
      ("merge", merge_command) ]

let bolt_command =
  Command.group ~summary:"BOLT vs. OCamlFDO. NOT IMPLEMENTED."
    ~readme:(fun () ->
      "Well, actually, it is implemented and we used it with an older \
       version of BOLT,\n\
       but it is not tested with the latest BOLT, and so the functionality\n\
       which is present in the tool isn't expose to the users at the moment.")
    []

let misc_command =
  Command.group
    ~summary:"Experimental commands and testing/debuging utilities"
    [ ("randomize-function-layout", randomize_function_order_command);
      ("bolt", bolt_command) ]

let main_command =
  Command.group ~summary:"Feedback-directed optimizer for Ocaml"
    ~readme:(fun () ->
      "decode: parses perf.data to generate a profile using debug info in \
       the executable. \n\
       opt: transforms intermediate IR using a profile\n\n\
       Important: ocamlfdo relies on compiler-libs and thus the same build \
       of ocamlopt must be used for building both ocamlfdo and the \
       executable.")
    [ ("decode", decode_command);
      ("opt", opt_command);
      ("linker-script", linker_script_command);
      ("compile", compile_command);
      ("check", check_command);
      ("profile", profile_command);
      ("misc", misc_command) ]

let run ?version ?build_info () =
  set_verbose false;
  Command.run ?version ?build_info main_command
