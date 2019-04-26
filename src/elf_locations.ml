[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = {
  map : Owee_buf.t;
  sections : Owee_elf.section array;
  strtab : Owee_elf.String_table.t;
  symtab : Owee_elf.Symbol_table.t;
  resolved : (Int64.t, (string * int) option) Hashtbl.t;
  resolved_fun : (Int64.t, string option) Hashtbl.t;
  mutable hits : int;
  mutable misses : int;
  mutable fun_hits : int;
  mutable fun_misses : int;
}

let verbose = true

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
  let resolved_fun = Hashtbl.create 42 in
  let strtab = Owee_elf.find_string_table map sections in
  let symtab = Owee_elf.find_symbol_table map sections in
  match symtab, strtab with
  | None, _ -> failwith "Can't find symbol table in elf binary"
  | _, None -> failwith "Can't find string table in elf binary"
  | Some symtab, Some strtab -> { map;
                                  sections;
                                  strtab;
                                  symtab;
                                  resolved;
                                  resolved_fun;
                                  hits = 0;
                                  misses = 0;
                                  fun_hits = 0;
                                  fun_misses = 0;
                                }

(* CR mshinwell: tidy all this up.  Also, the pinpointing of which row
   is the correct one isn't great. *)

type l = { filename : string option;
           state: Owee_debug_line.state }

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
              (state: Owee_debug_line.state)
              (prev:l option) : l option  =
          if state.end_sequence then
            None
          else begin
            let filename = Owee_debug_line.get_filename header state in
            let cur = { filename;
                        state = Owee_debug_line.copy state;
                      } in
            f cur prev;
            Some cur
          end
        in
        ignore (Owee_debug_line.fold_rows (header, chunk) check None);
        aux ()
    in
    aux ()

let print cur _ =
    match cur.filename with
    | None -> ()
    | Some filename ->
      Printf.printf "%s\t%d\t0x%Lx\n"
        filename cur.state.line cur.state.address

let print_dwarf t =
  resolve_from_dwarf t ~f:print

exception FoundLoc of string * int
exception FinishedFunc

let find_range t ~start ~finish ~fill_gaps cur prev =
  (* if verbose then
   *   Printf.printf "Find range for cache: (0x%Lx,0x%Lx):0x%Lx\n"
   *     start finish state.address; *)
  match prev with
  | None -> ()
  | Some prev ->
    if finish <= prev.state.address then
      raise FinishedFunc;
    if start <= prev.state.address then begin
      let filename =
        match prev.filename with
        | None ->
          if verbose then
            Printf.printf "find_range filename=None\n";
          prev.state.filename
        | Some filename ->
          if verbose then
            Printf.printf "find_range filename=%s\n" filename;
          filename
      in
      if verbose then
        Printf.printf "Caching loc 0x%Lx %s %d\n"
          prev.state.address filename prev.state.line;
      let res = (Some (filename,prev.state.line)) in
      if fill_gaps then begin
        let n =
          if cur.state.address < finish then cur.state.address
          else finish
        in
        let i = ref prev.state.address in
        while !i < n do
          Hashtbl.add t.resolved !i res;
          i := Int64.add !i 1L
        done
      end else begin
        Hashtbl.add t.resolved prev.state.address res
      end
    end

let resolve_function t ~sym =
  (* find function addresses *)
  let start = Owee_elf.Symbol_table.Symbol.value sym in
  let size = Owee_elf.Symbol_table.Symbol.size_in_bytes sym in
  let finish = Int64.add start size in
  if verbose then
    Printf.printf "Resolving function for cache: (0x%Lx,0x%Lx,0x%Lx)\n"
      start size finish;
  (* CR gyorsh: consider turning off fill_gaps for functions that are very
     long or do not have any linear ids. *)
  (* find dwarf locations for this function *)
  try
    resolve_from_dwarf t ~f:(find_range t ~start ~finish ~fill_gaps:true)
  with FinishedFunc -> ()

let find ~program_counter cur prev =
  match prev with
  | None -> ()
  | Some prev ->
    if program_counter >= prev.state.address
    && program_counter < cur.state.address then begin
      if verbose then begin
        Printf.printf "find prev.state.filename=%s .addr=0x%Lx\n"
          prev.state.filename prev.state.address;
        Printf.printf "find cur.state.filename=%s .addr=0x%Lx\n"
          cur.state.filename cur.state.address;
      end;
      match prev.filename with
      | None ->
        if verbose then
          Printf.printf "find filename=None\n";
        raise (FoundLoc (prev.state.filename, prev.state.line))
      | Some filename ->
        if verbose then
          Printf.printf "find filename=%s\n" filename;
        raise (FoundLoc (filename, prev.state.line))
    end

let resolve_pc t ~program_counter =
  try
    resolve_from_dwarf t ~f:(find ~program_counter);
    Hashtbl.add t.resolved program_counter None;
    None
  with (FoundLoc (filename, line)) ->
    let result = Some (filename, line) in
    if verbose then
      Printf.printf "Caching loc 0x%Lx %s %d\n"
        program_counter filename line;
    Hashtbl.add t.resolved program_counter result;
    result

let reset_cache t =
  let report msg h m =
    let hits = float_of_int h in
    let misses = float_of_int m in
    let ratio =
      if (misses +. hits) > 0. then
        hits /. (misses +. hits)
      else
        0.
    in
    Printf.printf "Cache %s: hit=%d, miss=%d, hit/(miss+hit)=%.3f\n"
      msg h m ratio
  in
  if verbose then begin
    report "loc" t.hits t.misses;
    report "fun" t.fun_hits t.fun_misses;
  end;
  Hashtbl.clear t.resolved;
  t.hits <- 0;
  t.misses <- 0

let resolve t ~program_counter =
  match Hashtbl.find t.resolved program_counter with
  | resolved ->
    t.hits <- t.hits + 1;
    if verbose then
      Printf.printf "Found loc in cache 0x%Lx\n" program_counter;
    resolved
  | exception Not_found ->
    t.misses <- t.misses + 1;
    if verbose then
      Printf.printf "Caching loc 0x%Lx\n" program_counter;
    resolve_pc t ~program_counter

let function_at_pc t ~program_counter:address =
  match Owee_elf.Symbol_table.functions_enclosing_address t.symtab ~address with
  | [] -> None
  (* Just take the first one for the moment.  There will usually be
     only one. *)
  | sym::_ -> Owee_elf.Symbol_table.Symbol.name sym t.strtab

let resolve_function_starting_at t ~program_counter =
  let report msg name =
    if verbose then
      Printf.printf "%s 0x%Lx:%s\n" msg program_counter
        (match name with
         | None -> "none"
         | Some n -> n)
  in
  report "Resolve_function_starting_at_pc:" None;
  match Hashtbl.find t.resolved_fun program_counter with
  | name ->
    t.fun_hits <- t.fun_hits + 1;
    report "Found fun in cache" name;
    name
  | exception Not_found ->
    t.fun_misses <- t.fun_misses + 1;
    let syms = Owee_elf.Symbol_table.functions_enclosing_address
                 t.symtab
                 ~address:program_counter in
    let rec find_func syms =
      match syms with
      | [] -> None
      | sym::tail ->
        let start = Owee_elf.Symbol_table.Symbol.value sym in
        if verbose then
          Printf.printf "Find func sym: start=0x%Lx pc=0x%Lx\n"
            start program_counter;
        (* Look for symbol whose start address is program counter.
           This is needed because functions_enclosing_address
           is based on start+size of symbols, and sometimes previous
           symbol's end of interval covers the start of the next symbol.
           This may be a bug in Owee, or maybe intentional,
           but we can work around it here. *)
        if start = program_counter then begin
          (* Once we have completed processing a function,
             we never go back to its addresses again. *)
          reset_cache t;
          resolve_function t ~sym;
          Owee_elf.Symbol_table.Symbol.name sym t.strtab
        end else find_func tail
    in
    let name = find_func syms in
    report "Caching fun " name;
    Hashtbl.add t.resolved_fun program_counter name;
    name
