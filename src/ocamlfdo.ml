open Core
open Main_fdo

let quiet () =
  verbose := false;
  Perf_profile.verbose := false;
  Aggregated_decoded_profile.verbose := false;
  Aggregated_perf_profile.verbose := false;
  Bolt_profile.verbose := false;
  Decoded_bolt_profile.verbose := false;
  Cfg_info.verbose := false;
  Clusters.verbose := false;
  Elf_locations.verbose := false;
  Ocaml_locations.verbose := false;
  Reorder.verbose := false;
  Report.verbose := false;
  Ocamlcfg.Cfg_builder.verbose := false;
  Crcs.verbose := false;
  Wrapper.verbose := false;
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
  let of_string s = List.find_exn M.all ~f:(fun t -> s = M.to_string t)

  let alternatives heading =
    let names = List.map M.all ~f:M.to_string in
    let default = M.(to_string default) in
    sprintf "%s: %s (default: %s)" heading
      (String.concat ~sep:"," names)
      default

  let mk name ~doc =
    assert (not (List.contains_dup ~compare M.all));
    Command.Param.(
      flag name
        (optional_with_default M.default (Command.Arg_type.create of_string))
        ~doc:(alternatives doc))
end

module Commonflag = struct
  type t = {
    name : string;
    doc : string;
    aliases : string list;
  }

  let optional t =
    Command.Param.(
      flag ~aliases:t.aliases t.name (optional Filename.arg_type) ~doc:t.doc)

  let required t =
    Command.Param.(
      flag ~aliases:t.aliases t.name (required Filename.arg_type) ~doc:t.doc)

  let flag_binary_filename =
    {
      name = "-binary";
      doc = "filename elf binary to optimize";
      aliases = [];
    }

  let flag_perf_profile_filename =
    {
      name = "-perf-profile";
      doc = "perf.data output of perf record";
      aliases = [];
    }

  let flag_linearid_profile_filename =
    {
      name = "-fdo-profile";
      doc = "filename decoded profile";
      aliases = [];
    }

  let flag_output_filename =
    { name = "-o"; doc = "filename output"; aliases = [] }

  let flag_linker_script_template_filename =
    {
      name = "-linker-script-template";
      doc = "filename linker script template";
      aliases = [];
    }

  let flag_linker_script_hot_filename =
    {
      name = "-linker-script-hot";
      doc = "filename hot functions layout for linker script";
      aliases = [];
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
  Command.Param.(flag "-verbose" ~aliases:[ "-v" ] no_arg ~doc:" verbose")

let flag_q = Command.Param.(flag "-q" no_arg ~doc:" quiet")

let flag_expected_pids =
  Command.Param.(
    flag "-pids"
      (optional_with_default []
         (Arg_type.comma_separated ~allow_empty:false ~strip_whitespace:true
            ~unique_values:true int))
      ~doc:
        "pids include samples only from these pids, specified as a \
         comma-separated list of integers")

let flag_buildid =
  Command.Param.(
    flag "-ignore-buildid" no_arg
      ~doc:" ignore mismatch in buildid between binary and perf.data")

let flag_force =
  Command.Param.(
    flag "-f" no_arg ~doc:" no assertions in linker-script-hot")

let flag_write_linker_script_hot =
  Command.Param.(
    flag "-write-linker-script-hot" no_arg
      ~doc:" write hot functions layout for linker script to a file")

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
        \         Given -fdo-profile <file>, if <file> does not exist, \
         then add -extra-debug. Without -fdo-profile, invoke ocamlopt \
         without splitting it into phases (ignore phase-specific ocamlfdo \
         arguments).")

let flag_crc =
  Command.Param.(
    flag "-md5" no_arg
      ~doc:
        " use md5 to detect source changes at function and compilation \
         unit level (implies both -md5-unit and -md5-fun)")

let flag_unit_crc =
  Command.Param.(
    flag "-md5-unit" no_arg
      ~doc:" use md5 per compilation unit to detect source changes")

let flag_func_crc =
  Command.Param.(
    flag "-md5-fun" no_arg
      ~doc:" use md5 per function to detect source changes")

let anon_files =
  Command.Param.(anon (sequence ("input" %: Filename.arg_type)))

let flag_reorder_blocks =
  let module RB = AltFlag (Config_reorder.Reorder_blocks) in
  RB.mk "-reorder-blocks"
    ~doc:"heuristics for reordering basic blocks of a function"

let flag_reorder_functions =
  let module RF = AltFlag (Config_reorder.Reorder_functions) in
  RF.mk "-reorder-functions"
    ~doc:"heuristics used for function layout generated in linker script"

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
       With -write-linker-script-hot, ocamlfdo decode will also produce \
       hot function layout in prog.exe.linker-script-hot file.\n")
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and binary_filename = Commonflag.(required flag_binary_filename)
      and perf_profile_filename =
        Commonflag.(required flag_perf_profile_filename)
      and reorder_functions = flag_reorder_functions
      and linearid_profile_filename =
        Commonflag.(optional flag_linearid_profile_filename)
      and linker_script_hot_filename =
        Commonflag.(optional flag_linker_script_hot_filename)
      and write_linker_script_hot = flag_write_linker_script_hot
      and buildid = flag_buildid
      and expected_pids = flag_expected_pids
      and force = flag_force in
      verbose := v;
      if q then quiet ();
      if !verbose && not write_linker_script_hot then
        printf
          "Ignoring -reorder-functions when -write-linker-script-hot is \
           not provided.\n";
      fun () ->
        decode ~binary_filename ~perf_profile_filename ~reorder_functions
          ~linker_script_hot_filename ~linearid_profile_filename
          ~write_linker_script_hot ~buildid ~expected_pids
          ~check:(not force))

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
       to optimize foo.cmir-linear and bar.cmir-linear and save the result \
       to\n\
       foo.cmir-linear-fdo and bar.cmir-linear-fdo.\n\
       The intermediate representation .cmir-linear files can be obtained by\n\
       $ ocamlopt -save-ir-after linearize foo.ml bar.ml <other options>\n\
       The decoded profile can be obtained by running \"ocamlfdo decode\".")
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and extra_debug = flag_extra_debug
      and fdo_profile = Commonflag.(optional flag_linearid_profile_filename)
      and reorder_blocks = flag_reorder_blocks
      and report = flag_report
      and unit_crc = flag_unit_crc
      and func_crc = flag_func_crc
      and crc = flag_crc
      and files = anon_files in
      verbose := v;
      if q then quiet ();
      let unit_crc = unit_crc || crc in
      let func_crc = func_crc || crc in
      if !verbose && List.is_empty files then printf "No input files\n";
      fun () ->
        optimize files ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
          ~func_crc ~report)

let check_command =
  Command.basic
    ~summary:
      "Check that transformation from Linear IR to CFG and back is identity."
    ~readme:(fun () ->
      {|
        If there is any difference in the function,
        dump Linear IR before and after the transformation to files.
        There can be a differences due to dead code, even though
        dead code elimination is not performed during this check.
|})
    Command.Let_syntax.(
      let%map v = flag_v and q = flag_q and files = anon_files in
      verbose := v;
      if q then quiet ();
      if !verbose && List.is_empty files then printf "No input files\n";
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
      and extra_debug = flag_extra_debug
      and auto = flag_auto
      and fdo_profile = Commonflag.(optional flag_linearid_profile_filename)
      and reorder_blocks = flag_reorder_blocks
      and report = flag_report
      and unit_crc = flag_unit_crc
      and func_crc = flag_func_crc
      and crc = flag_crc
      and args =
        Command.Param.(
          flag "--" escape
            ~doc:"ocamlopt_args standard options passed to ocamlopt")
      in
      verbose := v;
      if q then quiet ();
      let unit_crc = unit_crc || crc in
      let func_crc = func_crc || crc in
      let fdo =
        if auto then
          match fdo_profile with
          | None ->
              if !verbose then
                printf
                  "Missing -fdo-profile <file>, required for compilation\n\
                   when -auto is used. Calling ocamlopt directly, without\n\
                  \ splitting compilation into phases, and not \
                   intermediate IR is saved. All phase-specific arguments \
                   are ignored.\n";
              None
          | Some file ->
              if Sys.file_exists_exn file then (
                if !verbose then
                  printf
                    "With -auto, detected that -fdo-profile <%s> file does \
                     not exist.\n\n\
                    \ Setting -extra-debug to true."
                    file;
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
            compile args ~fdo_profile ~reorder_blocks ~extra_debug ~unit_crc
              ~func_crc ~report)

let linker_script_command =
  Command.basic
    ~summary:
      "Create linker script from a template and a layout of hot functions."
    ~readme:(fun () ->
      "Inserts the hot functions from linker-script-hot into the template \
       linker script, replacing the marker\n\
       INCLUDE linker-script-hot\n\n\
       A linker script can be obtained by running the command \"ld \
       --verbose\" and patching the output. See the default template, \
       distributed with the tool, in resources directory.\n\n\
       Use \"ocamlfdo decode\" to generate hot function layout.\n\
       This command performs a trivial transformation on the files.\n\
       It is useful when the linker runs from a different directory than \
       the one where the linker-script-hot file resides, such as when \
       ocamlfdo is used in a build system.\n\n\
       At most one of -fdo-profile and -linker-script can be provided.\n\
       If -fdo-profile is provided, hot functions layout is computed from \
       the profile\n\
       using a strategy specified by -reorder-functions, with default \
       strategy being\n\
       in the order of function execution counts. \n\
       If -linker-script-hot is provided, function layout is read from \
       that file.\n\
      \             Without -linker-script-hot and -fdo-profile arguments, \
       the marker is simply removed.\n\
      \       Argument of -linker-script-hot option must be a file that \
       contains\n\
       a valid fragment of linker script syntax. It can be empty. If \
       section names\n\
      \        listed in the file do not exist during linking, they are \
       simply ignored\n\
      \        without a warning. It can happen when recompiling after \
       source code change,\n\
      \        the numbers at the end of the function symbols can change.\n")
    Command.Let_syntax.(
      let%map v = flag_v
      and q = flag_q
      and output_filename = Commonflag.(optional flag_output_filename)
      and linker_script_template =
        Commonflag.(optional flag_linker_script_template_filename)
      and linker_script_hot =
        Commonflag.(optional flag_linker_script_hot_filename)
      and linearid_profile_filename =
        Commonflag.(optional flag_linearid_profile_filename)
      and reorder_functions = flag_reorder_functions
      and force = flag_force in
      verbose := v;
      if q then quiet ();
      if
        Option.is_some linearid_profile_filename
        && Option.is_some linker_script_hot
      then
        raise
          (Failure
             "Please provide at most one of -fdo-profile and \
              -linker-script-hot");
      if !verbose && Option.is_some linker_script_hot then
        printf
          "Ignoring -reorder-functions when -linker-script-hot is provided.\n";
      fun () ->
        linker_script ~output_filename ~linker_script_template
          ~linker_script_hot ~linearid_profile_filename ~reorder_functions
          ~check:(not force))

let main_command =
  Command.group ~summary:"Feedback-directed optimizer for Ocaml"
    ~readme:(fun () ->
      "\n\
       decode: parses perf.data to generate a profile using debug info in \
       the executable. \n\
       opt: transforms intermediate IR using a profile\n\n\
       Important: ocamlfdo relies on compiler-libs and thus the same build \
       of ocamlopt must be used for building both ocamlfdo and the \
       executable.")
    [
      ("decode", decode_command);
      ("opt", opt_command);
      ("linker-script", linker_script_command);
      ("compile", compile_command);
      ("check", check_command);
    ]

let () = Command.run main_command
