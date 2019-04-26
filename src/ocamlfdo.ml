open Core

let verbose = ref false

module Raw_layout : sig
  (* CR gyorsh: it has to be named t for csv  fields ppx to work. move to separate
     module. *)
  type t = {
    address : int64; (* start of function *)
    offset : int; (* offset from address *)
    index : int; (* index in the original layout *)
    position : int; (* new position in the permutation *)
  } [@@deriving fields, csv, compare, sexp]

  (* We don't need all the fields, but redundancy is used for validating
     the input. *)
  type p = t list [@@deriving sexp]

  val read : string -> p
  val print : p -> unit
  val print_t : t -> unit

end = struct
  type t = {
    address : int64; (* start of function *)
    offset : int; (* offset from address *)
    index : int; (* index in the original layout *)
    position : int; (* new position in the permutation *)
  } [@@deriving fields, csv, compare, sexp]

  type p = t list [@@deriving sexp]

  let print_t i =
    Printf.printf "0x%Lx:0x%x:%d:%d\n" i.address i.offset i.position i.position

  let print l =
    (* List.iter l ~f:print_t; *)
    Printf.printf !"%{sexp:p}\n" l

  let read filename =
    if !verbose then
      printf "Reading raw layout from %s\n" filename;
    let p = csv_load ~separator:':' filename in
    if !verbose then print p;
    p

end

module Rel_layout : sig
  type t = {
    func : string;
    labels : int list;
  } [@@deriving sexp]

  type p = t list [@@deriving sexp]

  val read : string -> p

  (* Write a single function layout *)
  val writer : string option -> (string -> int list -> unit)

end = struct

  type t = {
    func : string;
    labels : int list;
  } [@@deriving sexp]

  type p = t list [@@deriving sexp]

  let print_t ~t outc =
    fprintf outc !"%{sexp:t}\n" t

  let print_p p outc =
    List.iter p ~f:(fun t -> print_t ~t outc)

  let read filename =
    if !verbose then
      printf "Reading layout from %s\n" filename;

    let p =
      match Parsexp_io.load (module Parsexp.Many) ~filename with
      | Ok p_sexp_list -> List.map p_sexp_list ~f:t_of_sexp
      | Error error ->
        Parsexp.Parse_error.report Caml.Format.std_formatter error ~filename;
        failwith "Cannot parse relative layout file"
    in
    (* let p = csv_load ~separator:':' filename in *)
    if !verbose then begin
      print_p p Out_channel.stdout;
      if p = [] then Printf.printf "Empty layout!\n"
    end;
    p

  let writer filename =
    match filename with
      | None -> (fun _ _ -> ())
      | Some filename ->
        (* CR gyorsh: fix this ugly hack to create an empty file
           and then append to it, if the file exists. *)
        Out_channel.close_no_err (Out_channel.create filename);
        (fun func labels ->
          let t = { func; labels; } in
          Out_channel.with_file filename ~f:(print_t ~t)
            ~binary:false
            ~append:true)
end

let load_locations binary_filename =
  let elf_locations = Elf_locations.create ~elf_executable:binary_filename in
  (* if !verbose then Elf_locations.print_dwarf elf_locations; *)
  elf_locations

let decode_perf_data _locations filename =
  if !verbose then
    printf "Reading perf profile from %s\n" filename;
  failwith "Not implemented: read_perf_data"

let print_fun_layout_item (key, data) =
  Printf.printf "position=%d linear_id=%d\n" key data

let print_fun_layout ~key:name ~data:(fun_layout : (int,int) Hashtbl.t) =
  Printf.printf "%s (%d)\n" name (Hashtbl.length fun_layout);
  let sorted_fun_layout =
    List.sort (Hashtbl.to_alist fun_layout)
      ~compare:(fun (k1, _) (k2,_) -> Int.compare k1 k2)
  in
  List.iter sorted_fun_layout ~f:print_fun_layout_item

let print_layout layout =
  Hashtbl.iteri layout ~f:print_fun_layout

let _to_func file =
  match String.chop_suffix file ~suffix:".linear" with
  | None -> None
  | Some name ->
    let symbol_prefix =
      if X86_proc.system = X86_proc.S_macosx then "_" else ""
    in
    Some (X86_proc.string_of_symbol symbol_prefix name)

(* Use addresses from permutation locations to find linear id layout *)
let decode_layout locations permutation =
  let layout = Hashtbl.create (module String) in
  let add func pos id =
    let fun_layout =
      Hashtbl.find_or_add layout func
        ~default:(fun () -> Hashtbl.create (module Int)) in
    match Hashtbl.add fun_layout ~key:pos ~data:id with
    | `Duplicate -> failwithf
                      "Cannot add linear_id %d at position %d in function %s"
                      id pos func ()
    | `Ok ->
      if !verbose then
        Printf.printf "Adding %s %d %d\n" func pos id;
      ()
  in
  (* CR gyorsh: optimize reading locations and raw_layouts:
     scan the layout file for function start addresses,
   * then scan the debug info from the binary and check for each of the function
   * addresses if there are any locations with ".linear" filename and
   * only for those cache them in a way that everything else returns
   * null. *)
  let decode_item (l:Raw_layout.t) =
    if !verbose then Raw_layout.print_t l;
    let func = Elf_locations.resolve_function_starting_at locations
                 ~program_counter:l.address
    in begin
      match func with
      | None ->
        if !verbose then
          Printf.printf "NOT FOUND %Lx\n" l.address;
      | Some func -> begin
          if !verbose then begin
            Printf.printf "Function %s\n" func;
            Raw_layout.print_t l
          end;
          let program_counter =
            Int64.(l.address + (Int64.of_int l.offset)) in
          let loc = Elf_locations.resolve locations ~program_counter in
          match loc with
          | None ->
            if !verbose then
              Printf.printf "Elf location NOT FOUND at 0x%Lx\n"
                program_counter
          | Some (file,line) ->
            if !verbose then
              Printf.printf "%s:%d\n" file line;
            (* Check that the func symbol name from the binary where
               permutation comes from matches the function name encoded
               as filename into our special dwarf info. *)
            match String.chop_suffix file ~suffix:".linear" with
            | None ->
              if !verbose then
                Printf.printf "Ignoring %s in %s\n" func file;
            | Some func_name_dwarf -> begin
                if func_name_dwarf = func then
                  add func l.position line
                else
                  failwithf "func_name_dwarf = %s func = %s\n"
                    func_name_dwarf func ()
              end
        end
    end
  in
  List.iter permutation ~f:decode_item;
  if !verbose then print_layout layout;
  let sort fun_layout =
    List.sort (Hashtbl.to_alist fun_layout)
      ~compare:(fun (k1, _) (k2,_) -> Int.compare k1 k2)
    |> List.map ~f:(fun (_k, d) -> d)
  in
  let layout = Hashtbl.map layout ~f:sort in
  if Hashtbl.is_empty layout &&
     not (List.is_empty permutation) then
    failwith "Cannot decode layout\n";
  layout

let convert_layout (l:Rel_layout.p) =
  let layout = Hashtbl.create (module String) in
  List.iter l
    ~f:(fun p ->
      Hashtbl.add_exn layout ~key:p.func ~data:p.labels);
  layout

let setup_reorder ~binary_filename
      ~perf_profile_filename
      ~raw_layout_filename
      ~rel_layout_filename
      ~linearid_layout_filename
      ~random_order
      w =
  if random_order then
    Reorder.Random
  else begin
    match binary_filename with
    | None -> begin
        match rel_layout_filename with
        | Some rel_layout_filename ->
          let layout =
            convert_layout (Rel_layout.read rel_layout_filename) in
          Reorder.Cfg_label layout
        | None ->
          match linearid_layout_filename with
          | Some linearid_layout_filename ->
            let layout =
              convert_layout (Rel_layout.read linearid_layout_filename) in
            Reorder.Linear_id layout
          | None -> Reorder.Identity
      end
    | Some binary_filename -> begin
        let locations = load_locations binary_filename in
        match raw_layout_filename with
        | Some raw_layout_filename ->
          let raw_layout = Raw_layout.read raw_layout_filename in
          let layout = decode_layout locations raw_layout  in
          (* Save decoded layout *)
          Hashtbl.iteri layout ~f:(fun ~key ~data ->
            if not (List.is_empty data) then w key data);
          Reorder.Linear_id layout
        | None -> begin
            match perf_profile_filename with
            | Some perf_profile_filename -> begin
                let perf_data =
                  decode_perf_data locations perf_profile_filename in
                Reorder.CachePlus perf_data
              end
            | None -> Reorder.Identity
          end
      end
  end


let call_ocamlopt args =
  (* Set debug "-g" to emit dwarf locations. *)
  Clflags.debug := true;
  Clflags.extended_debug := true;
  (* set command line to args to call ocamlopt *)
  begin match args with
  | None | Some [] ->
    if !verbose then
      printf "Missing compilation command\n"
  | Some args ->
    if !verbose then begin
      printf "ocamlopt ";
      List.iter ~f:(fun s -> printf " %s" s) args;
      printf "\n"
    end
  end;
  let args = (Array.of_list (Option.value args ~default:[])) in
  let len = Array.length args in
  let argc = Array.length Sys.argv in
  assert (len < argc);
  Array.blit ~src:args ~src_pos:0 ~dst:Sys.argv ~dst_pos:1 ~len;
  (* CR gyorsh: Can't resize args array. Fake missing arguments. Better way? *)
  Array.fill Sys.argv ~pos:(len+1) ~len:(argc-len-1) "-inlining-report";
  Optmain.main ()

let check_equal f ~new_body =
  let open Linearize in
  let rec equal i1 i2 =
    (* Format.kasprintf prerr_endline "@;%a" Printlinear.instr i1;
     * Format.kasprintf prerr_endline "@;%a" Printlinear.instr i2; *)
    if i1.desc = i2.desc &&
       i1.id = i2.id &&
       Reg.array_equal i1.arg i2.arg &&
       Reg.array_equal i1.res i2.res &&
       Reg.Set.equal i1.live i2.live &&
       (Debuginfo.compare i1.dbg i2.dbg) = 0
    then begin
      if i1.desc = Lend then true
      else equal i1.next i2.next
    end
    else begin
      Format.kasprintf prerr_endline "Equality failed on:@;%a@;%a"
        Printlinear.instr i1
        Printlinear.instr i2;
      false
    end
  in
  if not (equal f.fun_body new_body) then begin
    Format.kasprintf prerr_endline "Before:@;%a"
      Printlinear.fundecl f;
    Format.kasprintf prerr_endline "\nAfter:@;%a"
      Printlinear.fundecl {f with fun_body = new_body};
    failwith "Conversion from linear to cfg and back to linear \
              is not an identity function.\n"
  end

let print_linear msg f =
  if false then
  if !verbose then begin
    Printf.printf "%s processing %s\n" f.Linearize.fun_name msg;
    Format.kasprintf prerr_endline "@;%a" Printlinear.fundecl f
  end

let main ~binary_filename
      ~perf_profile_filename
      ~raw_layout_filename
      ~rel_layout_filename
      ~linearid_layout_filename
      ~gen_rel_layout
      ~gen_linearid_layout
      ~random_order
      ~eliminate_dead_blocks
      args =

  let w_linearid = Rel_layout.writer gen_linearid_layout in
  let algo = setup_reorder ~binary_filename
               ~perf_profile_filename
               ~raw_layout_filename
               ~rel_layout_filename
               ~linearid_layout_filename
               ~random_order
               w_linearid
  in
  let reorder = Reorder.reorder ~algo in
  let w_rel = Rel_layout.writer gen_rel_layout in
  let write_rel_layout new_cfg =
    let new_layout = Cfg.get_layout new_cfg in
    let fun_name = Cfg.get_name new_cfg in
    w_rel fun_name new_layout
  in
  let validate f ~new_body =
    match algo with
    | Reorder.Identity ->
      if eliminate_dead_blocks then ()
      else check_equal f ~new_body
    | _ -> ()
  in
  let transform f =
    print_linear "Before" f;
    let cfg = Cfg.from_linear f in
    let new_cfg = reorder cfg in
    write_rel_layout new_cfg;
    (* If we eliminate dead blocks before a transformation
       then some algorithms might not apply because they rely
       on perf data based on original instructions. *)
    if eliminate_dead_blocks then
      Cfg.eliminate_dead_blocks cfg;
    let new_body = Cfg.to_linear new_cfg in
    validate f ~new_body;
    let fnew = {f with fun_body = new_body} in
    print_linear "After" fnew;
    fnew
  in
  Reoptimize.setup ~f:transform;
  call_ocamlopt args

let command =
  Command.basic
    ~summary:"Feedback-directed optimizer for Ocaml"
    ~readme:(fun () ->
      "
Build your program with ocamlfdo to enable extra debug info
for low-level optimizations (currently, only linearize pass):
$ ocamlfdo -- <standard ocamlopt options including -o prog.exe>

Collect perf profile with LBR information:
$ perf record -e cycles:u -j any,u -o perf.data <prog.exe> <args..>

Run ocamlfdo with exactly the same version of the source code and options as above:
$ ocamlfdo -perf-profile <perf.data> -binary <prog.exe> -- \\
           <standard ocamlopt options including -o prog.fdo.exe>

Important: ocamlfdo relies on compiler-libs and thus the build of ocamlfdo must
match the build of ocamlopt used above.
"
    )
    Command.Let_syntax.(
      let%map_open
        v = flag "-v" no_arg ~doc:" verbose"
      and q = flag "-q" no_arg ~doc:" quiet"
      and eliminate_dead_blocks =
        flag "-edb" no_arg ~doc:" eliminate dead blocks"
      and gen_rel_layout =
        flag "-gen-layout" (optional Filename.arg_type)
          ~doc:"filename generate relative layout and write to <filename>"
      and gen_linearid_layout =
        flag "-gen-linearid-layout" (optional Filename.arg_type)
          ~doc:"filename generate relative layout and write to <filename>"
      and random_order =
        flag "-random-order" no_arg ~doc:" reorder blocks at random"
      and binary_filename =
        flag "-binary" (optional Filename.arg_type)
          ~doc:"filename elf binary to optimize"
      and perf_profile_filename =
        flag "-perf-profile" (optional Filename.arg_type)
          ~doc:"perf.data output of perf record"
      and raw_layout_filename =
        flag "-raw-layout" (optional Filename.arg_type)
          ~doc:"filename block layout for reordering: raw binary addresses"
      and rel_layout_filename =
        flag "-rel-layout" (optional Filename.arg_type)
          ~doc:"filename block layout for reordering relative to function start,
does not require -binary"
      and linearid_layout_filename =
        flag "-linearid-layout" (optional Filename.arg_type)
          ~doc:"filename same as -rel-layout but uses linear id not cfg labels"
      and args = flag "--" (escape)
                   ~doc:"ocamlopt_args standard options passed to opcamlopt"
      in
      if v then verbose := true;
      if q then verbose := false;

      (* CR gyorsh: use choose_one to reduce the mess below? *)
      if !verbose then begin
        if random_order then begin
          if not (perf_profile_filename = None) ||
             not (raw_layout_filename = None) ||
             not (rel_layout_filename = None) ||
             not (linearid_layout_filename = None) ||
             not (binary_filename = None)
          then begin
            Printf.printf
              "Warning: Ignoring -perf-profile -raw-layout -layout \
               -linearid-layout -binary. ";
            Printf.printf "Incompatible with -random\n";
          end
        end;
        if binary_filename = None then begin
          if not (perf_profile_filename = None) ||
             not (raw_layout_filename = None) then begin
            Printf.printf "Warning: ignoring -raw_layout and -perf-profile. ";
            Printf.printf "Cannot use without -binary.\n";
          end
        end else begin
          if (perf_profile_filename = None) &&
             (raw_layout_filename = None) then
            begin
              Printf.printf "Warning: Ignoring -binary. ";
              Printf.printf "Cannot use without -perf-profile or -raw-layout.\n";
            end
        end
      end;
      fun () -> main
                  ~binary_filename
                  ~perf_profile_filename
                  ~raw_layout_filename
                  ~rel_layout_filename
                  ~linearid_layout_filename
                  ~gen_rel_layout
                  ~gen_linearid_layout
                  ~random_order
                  ~eliminate_dead_blocks
                  args)

let () = Command.run command
