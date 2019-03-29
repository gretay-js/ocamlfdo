[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = {
  map : Owee_buf.t;
  sections : Owee_elf.section array;
  strtab : Owee_elf.String_table.t;
  symtab : Owee_elf.Symbol_table.t;
  resolved : (Int64.t, (string * int) option) Hashtbl.t;
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
  let strtab = Owee_elf.find_string_table map sections in
  let symtab = Owee_elf.find_symbol_table map sections in
  match symtab, strtab with
  | None, _ -> failwith "Can't find symbol table in elf binary"
  | _, None -> failwith "Can't find string table in elf binary"
  | Some symtab, Some strtab -> { map; sections; strtab; symtab; resolved; }

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
      Printf.printf "%s\t%d\t0x%x\n" filename state.line state.address

let print_dwarf t =
  resolve_from_dwarf t ~f:print

exception FoundLoc of string * int
exception FoundSym of Owee_elf.Symbol_table.Symbol.t
exception FinishedFunc

let find_range t ~start ~finish
      ~filename (state : Owee_debug_line.state) _ =
  let state_address = Int64.of_int state.address in
  if start >= state_address && finish < state_address then
    let result = match filename with
      | None -> state.filename, state.line
      | Some filename -> filename, state.line
    in
    Hashtbl.add t.resolved state_address (Some result)
  else if state_address <= finish then
    raise FinishedFunc

let resolve_function t ~name =
  (* find symbol for this function name *)
  let module S = Owee_elf.Symbol_table.Symbol in
  try
    Owee_elf.Symbol_table.iter t.symtab ~f:(fun sym ->
      match S.type_attribute sym with
      | Func -> begin
        match Owee_elf.Symbol_table.Symbol.name sym t.strtab with
        | Some ns -> if name = ns then raise (FoundSym (sym))
        | _ -> ()
      end
      | _ -> ());
    failwith "Function not found"
  with (FoundSym sym) -> begin
      (* find function addresses *)
      let start = S.value sym in
      let size = S.size_in_bytes sym in
      let finish = Int64.add start size in
      (* find dwarf locations for this function *)
      begin try
        resolve_from_dwarf t ~f:(find_range t ~start ~finish)
      with FinishedFunc -> ()
      end;
      (* we found everything in the range, now fill in the gaps *)
      let prev = ref None in
      let address = ref start in
      while !address < finish do
        match Hashtbl.find t.resolved !address with
        | resolved -> prev := resolved
        | exception Not_found -> Hashtbl.add t.resolved !address !prev;
          address := Int64.add !address 1L
      done;
      (start, size)
    end

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
  (* CR-soon mshinwell: owee should use Int64.t *)
  let program_counter' = program_counter in
  let program_counter = Int64.to_int program_counter in
  try
    resolve_from_dwarf t ~f:(find ~program_counter);
    Hashtbl.add t.resolved program_counter' None;
    None
  with (FoundLoc (filename, line)) ->
    let result = Some (filename, line) in
    Hashtbl.add t.resolved program_counter' result;
    result

let resolve t ~program_counter =
  match Hashtbl.find t.resolved program_counter with
  | resolved -> resolved
  | exception Not_found -> resolve_pc t ~program_counter

let function_at_pc t ~program_counter:address =
  match Owee_elf.Symbol_table.functions_enclosing_address t.symtab ~address with
  | [] -> None
  (* Just take the first one for the moment.  There will usually be
     only one. *)
  | sym::_ -> Owee_elf.Symbol_table.Symbol.name sym t.strtab
