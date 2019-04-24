[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = {
  map : Owee_buf.t;
  sections : Owee_elf.section array;
  strtab : Owee_elf.String_table.t;
  symtab : Owee_elf.Symbol_table.t;
  resolved : (Int64.t, (string * int) option) Hashtbl.t;
  resolved_functions : (Int64.t, string option) Hashtbl.t;
  mutable hits : int;
  mutable misses : int;
}

let create ~elf_executable =
  let fd = Unix.openfile elf_executable [Unix.O_RDONLY] 0 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let map = Bigarray.array1_of_genarray
              (Unix.map_file fd
                 Bigarray.int8_unsigned
                 Bigarray.c_layout
                 false
                 [| len |])
  in
  Unix.close fd;
  let _header, sections = Owee_elf.read_elf map in
  let resolved = Hashtbl.create 42 in
  let resolved_functions = Hashtbl.create 42 in
  let strtab = Owee_elf.find_string_table map sections in
  let symtab = Owee_elf.find_symbol_table map sections in
  let hits =  0 in
  let misses = 0 in
  match symtab, strtab with
  | None, _ -> failwith "Can't find symbol table in elf binary"
  | _, None -> failwith "Can't find string table in elf binary"
  | Some symtab, Some strtab -> { map;
                                  sections;
                                  strtab;
                                  symtab;
                                  resolved;
                                  resolved_functions;
                                  hits;
                                  misses;
                                }

(* CR mshinwell: tidy all this up.  Also, the pinpointing of which row
   is the correct one isn't great. *)

let resolve_from_dwarf t ~f =
  match Owee_elf.find_section t.sections ".debug_line" with
  | None -> ()
  | Some section ->
    let body = Owee_buf.cursor (Owee_elf.section_body t.map section) in
    let rec aux () =
      match Owee_debug_line.read_chunk body with
      | None -> ()
      | Some (header, chunk) ->
        let check header
              (state : Owee_debug_line.state)
              (prev_state : Owee_debug_line.state option) =
          if not state.end_sequence then begin
            let filename = Owee_debug_line.get_filename header state
            in (f ~filename state prev_state)
          end;
           (* N.B. [state] is mutable! *)
          Some (Owee_debug_line.copy state)
        in
        ignore (Owee_debug_line.fold_rows (header, chunk) check None);
        aux ()
    in
    aux ()

let print ~filename (state : Owee_debug_line.state) _ =
    match filename with
    | None -> ()
    | Some filename ->
      Printf.printf "%s\t%d\t0x%Lx\n" filename state.line state.address

let print_dwarf t =
  resolve_from_dwarf t ~f:print

exception FoundLoc of string * int
exception FinishedFunc

let find_range t ~start ~finish
      ~filename (state : Owee_debug_line.state) _ =
  let state_address = state.address in
  if start >= state_address && finish < state_address then
    let result = match filename with
      | None -> state.filename, state.line
      | Some filename -> filename, state.line
    in
    Hashtbl.add t.resolved state_address (Some result)
  else if state_address <= finish then
    raise FinishedFunc

let _resolve_function t ~sym =
  (* find function addresses *)
  let start = Owee_elf.Symbol_table.Symbol.value sym in
  let size = Owee_elf.Symbol_table.Symbol.size_in_bytes sym in
  let finish = Int64.add start size in
  (* find dwarf locations for this function *)
  try
    resolve_from_dwarf t ~f:(find_range t ~start ~finish)
  with FinishedFunc -> ()

let find ~program_counter
      ~filename
      (state : Owee_debug_line.state)
      (prev_state : Owee_debug_line.state option) =
  match prev_state with
  | None -> ()
  | Some prev_state ->
    if program_counter >= prev_state.address
    && program_counter < state.address then
      match filename with
      | None -> raise (FoundLoc (prev_state.filename, prev_state.line))
      | Some filename -> raise (FoundLoc (filename, prev_state.line))

let resolve_pc t ~program_counter =
  try
    resolve_from_dwarf t ~f:(find ~program_counter);
    Hashtbl.add t.resolved program_counter None;
    None
  with (FoundLoc (filename, line)) ->
    let result = Some (filename, line) in
    Hashtbl.add t.resolved program_counter result;
    result

let reset_cache t =
  let hits = float_of_int t.hits in
  let misses = float_of_int t.misses in
  let ratio =
    if (misses +. hits) > 0. then
      hits /. (misses +. hits)
    else
      0.
  in
  Printf.printf "hit=%d, miss=%d, hit/(miss+hit)=%.3f\n"
    t.hits t.misses ratio;
  Hashtbl.clear t.resolved;
  t.hits <- 0;
  t.misses <- 0

let resolve t ~program_counter =
  match Hashtbl.find t.resolved program_counter with
  | resolved ->
    t.hits <- t.hits + 1;
    resolved
  | exception Not_found ->
    t.misses <- t.misses + 1;
    resolve_pc t ~program_counter

let function_at_pc t ~program_counter:address =
  match Owee_elf.Symbol_table.functions_enclosing_address t.symtab ~address with
  | [] -> None
  (* Just take the first one for the moment.  There will usually be
     only one. *)
  | sym::_ -> Owee_elf.Symbol_table.Symbol.name sym t.strtab

let resolve_function_at_pc t ~program_counter =
  match Hashtbl.find t.resolved_functions program_counter with
  | name -> name
  | exception Not_found ->
    let name =
      let syms = Owee_elf.Symbol_table.functions_enclosing_address
                   t.symtab
                   ~address:program_counter in
      match syms with
      | [] -> None
      | sym::_ ->
        reset_cache t;
        (* resolve_function t ~sym; *)
        Owee_elf.Symbol_table.Symbol.name sym t.strtab
    in
    Hashtbl.add t.resolved_functions program_counter name;
    name
