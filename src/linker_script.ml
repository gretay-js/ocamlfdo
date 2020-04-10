open Core
module AD = Aggregated_decoded_profile

let verbose = ref true

let hot_begin = "caml_hot__code_begin"

let hot_end = "caml_hot__code_end"

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
    fprintf oc "ASSERT (DEFINED(%s)?(%s <= %s) && (%s <= %s):1, \n%s);\n" n
      hot_begin n n hot_end (msg n)
  in
  let ocaml_cond n =
    fprintf oc "ASSERT ((%s <= %s) && (%s <= %s), \n%s);\n" hot_begin n n
      hot_end (msg n)
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

let save_hot filename functions ~check =
  if !verbose then printf "Writing linker script hot to %s\n" filename;
  Out_channel.with_file filename ~f:(print_hot ~functions ~check)

let write_hot filename profile ~reorder_functions ~check =
  let functions = Reorder.hot_functions profile ~reorder_functions in
  save_hot filename functions ~check

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
        let profile = AD.read_bin filename in
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

(* Use owee to load the symbols from the binary. Compute the expected hot
   function layout from the profile and the reordering strategy.

   Find all the function symbols between caml_hot__code_begin and
   caml_hot__code_end markers and check that they match the expected hot
   function layout.

   We make more than one pass on the binary's symbol table to get all the
   information, because Owee's interface for extracting symbols is somewhat
   limited.

   First find the addresses of all the function symbols in the expected hot
   functions layout, including hot code begin and end markers. Then, using
   hot code begin and end markers, find all the function symbols that appear
   between these markers. Compare the expect layout to the actual layout and
   report differences, including actually addresses of all the expected hot
   functions, to give better user messages. *)
type info =
  { missing : int;
    not_in_hot : string list
  }

let check_function_order ~binary_filename ~profile_filename
    ~reorder_functions ~output_filename =
  let locations = Elf_locations.create ~elf_executable:binary_filename in
  let profile = AD.read_bin profile_filename in
  let hot_wrap l = (hot_begin :: l) @ [hot_end] in
  (* Find addresses of all the function in the expected layout. *)
  let hot = Reorder.hot_functions profile ~reorder_functions in
  let expected = hot_wrap hot in
  let len = List.length expected in
  let name2addr = String.Table.create ~size:len () in
  List.iter expected ~f:(fun name ->
      match Hashtbl.add name2addr ~key:name ~data:None with
      | `Duplicate ->
          Report.user_error
            "Function %s appears more than once in hot layout.\n" name
      | `Ok -> ());
  (* hot_begin and hot_end are not func symbols, so we need both func and
     data set to true in the call to iter_symbols. *)
  Elf_locations.iter_symbols locations ~func:true ~data:true
    ~f:(fun name start ->
      match Hashtbl.find name2addr name with
      | Some None -> Hashtbl.set name2addr ~key:name ~data:(Some start)
      | Some (Some addr) ->
          (* every symbol shows up twice: dynamic and static symbol table *)
          if (not Raw_addr.(addr = start)) && !verbose then
            Printf.printf
              "find_functions surprised to see again %s at 0x%Lx, prev \
               address 0x%Lx\n"
              name start addr
      | None -> ());

  let get_addr name =
    match Hashtbl.find_exn name2addr name with
    | None ->
        if !verbose then Printf.printf "Missing function symbol %s\n" name;
        None
    | addr -> addr
  in
  (* Find all symbols between hot code begin and end markers. *)
  let hot_begin_addr = get_addr hot_begin in
  let hot_end_addr = get_addr hot_end in

  (* Is hot code begin marker aligned? *)
  let is_page_aligned addr =
    let open Raw_addr in
    let min_page_size = 4L * 1024L in
    if !verbose then
      Printf.printf
        "Checking alignment of 0x%Lx to a minimal page boundary of 4K bytes \
         (=0x%Lx)\n"
        addr min_page_size;
    Caml.Int64.unsigned_rem addr min_page_size = 0L
  in
  match (hot_begin_addr, hot_end_addr) with
  | None, _ | _, None ->
      Report.user_error
        "Missing some of the symbols %s and %s in the binary %s.\n\
         Hint: Function reordering requires ocaml compiler version 4.10 or \
         higher.\n\
         Was the binary compiled with an older version of ocaml\n\
         or not an ocaml binary?" hot_begin hot_end binary_filename
  | Some hot_begin_addr, Some hot_end_addr ->
      if (not (is_page_aligned hot_begin_addr)) && !verbose then
        Printf.printf "Warning: %s at address 0x%Lx is not page aligned.\n"
          hot_begin hot_begin_addr;
      let is_in_hot addr =
        Raw_addr.(hot_begin_addr <= addr && addr < hot_end_addr)
      in
      let addr2name = Raw_addr.Table.create ~size:len () in
      Elf_locations.iter_symbols locations ~func:true ~data:true
        ~f:(fun name start ->
          if is_in_hot start then
            Raw_addr.Table.update addr2name start ~f:(function
              | None -> [name]
              | Some names -> name :: names));
      (* Remove hot_begin so that we can put it first regardless of the sort
         of other function names at the same address, to make nice output.
         Alternative is to compare all names to it in the iter_symbol, which
         would be slower as there could be lots of symbols. *)
      Hashtbl.change addr2name hot_begin_addr ~f:(function
        | None -> None
        | Some names ->
            let new_names =
              List.filter names ~f:(fun n -> not (String.equal hot_begin n))
            in
            if List.is_empty new_names then None else Some new_names);
      (* If there is more than one symbol per address, sort by their position
         in the expect list, or if not in the expect, then alphabetically. *)
      let compare a b =
        let ia = List.findi expected ~f:(fun _ -> String.equal a) in
        let ib = List.findi expected ~f:(fun _ -> String.equal b) in
        match (ia, ib) with
        | Some (ia, _), Some (ib, _) ->
            let res = Int.compare ia ib in
            if res = 0 then String.compare a b else res
        | None, None -> String.compare a b
        | None, Some _ -> 1 (* prefer expected *)
        | Some _, None -> -1
      in
      (* get list of names sorted by addresses *)
      let actual =
        Map.of_hashtbl_exn (module Raw_addr) addr2name
        |> Map.data
        |> List.map ~f:(List.dedup_and_sort ~compare)
        |> List.concat |> hot_wrap
      in
      if List.compare String.compare expected actual = 0 then (
        if !verbose then (
          Printf.printf !"Success!\n";
          List.iter ~f:print_endline actual ) )
      else (
        (* print the differences *)
        if !verbose then
          Printf.printf
            "Differences between expected and actual layout of hot functions\n";
        (* print all extra functions that are unexpected in the hot segment *)
        let extra =
          List.fold actual ~init:0 ~f:(fun acc name ->
              match Hashtbl.find name2addr name with
              | Some _ -> acc
              | None ->
                  if !verbose then
                    Printf.printf
                      "Unexpected function symbol %s in hot segment.\n" name;
                  acc + 1)
        in
        if extra > 0 && !verbose then
          Printf.printf
            "%d unexpected function symbols appear in hot segment of \
             %s(%.3f%%).\n"
            extra binary_filename
            (Report.percent extra (len - 2));
        (* print all misplaced functions with addresses *)
        let { missing; not_in_hot } =
          List.fold hot ~init:{ missing = 0; not_in_hot = [] }
            ~f:(fun acc name ->
              match get_addr name with
              | None -> { acc with missing = acc.missing + 1 }
              | Some addr ->
                  if is_in_hot addr then acc
                  else (
                    if !verbose then
                      Printf.printf
                        "Hot symbol %s is at 0x%Lx, not in hot segment\n"
                        name addr;
                    { acc with not_in_hot = name :: acc.not_in_hot } ))
        in
        if missing > 0 && !verbose then
          Printf.printf
            "Missing %d hot function symbols in binary %s ((%.3f%%))\n"
            missing binary_filename
            (Report.percent missing (len - 2));
        let n = List.length not_in_hot in
        if n > 0 && !verbose then
          Printf.printf
            "Placed %d hot function symbols outside of hot code segment \
             [0x%Lx,0x%Lx] in binary %s (%.3f%%)\n"
            n hot_begin_addr hot_end_addr binary_filename
            (Report.percent n (len - 2));

        (* append not_in_hot to actual, in the order of their addresses, to
           show a nicer output with patdiff *)
        let actual =
          actual
          @ List.sort not_in_hot ~compare:(fun n1 n2 ->
                let a1 = Option.value_exn (Hashtbl.find_exn name2addr n1) in
                let a2 = Option.value_exn (Hashtbl.find_exn name2addr n2) in
                let res = Raw_addr.compare a1 a2 in
                if res = 0 then String.compare n1 n2 else res)
        in
        (* save expected and actual to two files, to make it easy for
           diffing. CR gyorsh: can we do it using patdiff programmatically? *)
        let output_filename =
          match output_filename with
          | None -> binary_filename
          | Some f -> f
        in
        let save ext layout =
          let filename = sprintf "%s.tmp.%s" output_filename ext in
          if !verbose then
            Printf.printf "Writing %s layout to %s\n" ext filename;
          Out_channel.with_file filename ~f:(fun oc ->
              Out_channel.output_lines oc layout);
          filename
        in
        let file1 = save "expected" expected in
        let file2 = save "actual" actual in
        Ppxlib_print_diff.print ~use_color:true ~file1 ~file2 ();
        exit (-1) )

let randomize_function_order ~binary_filename ~output_filename ~check =
  let locations = Elf_locations.create ~elf_executable:binary_filename in
  let layout = ref [] in
  Elf_locations.iter_symbols locations ~func:true ~data:false
    ~f:(fun name _ ->
      if not (String.contains name '@') then layout := name :: !layout);
  let layout = List.permute !layout in
  let output_filename =
    Option.value output_filename
      ~default:(binary_filename ^ ".linker-script-hot")
  in
  save_hot output_filename layout ~check
