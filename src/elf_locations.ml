(**************************************************************************)
(*                                                                        *)
(*                                 OCamlFDO                               *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*                     based on spacetime_lib                             *)
(*   Copyright (c) 2016 Leo White, Mark Shinwell                          *)
(*   https://github.com/lpw25/spacetime_lib                               *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(* CR-soon gyorsh: cleanup the file, we don't use all of the functions in
   it. *)
[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = {
  map : Owee_buf.t;
  sections : Owee_elf.section array;
  strtab : Owee_elf.String_table.t;
  symtab : Owee_elf.Symbol_table.t;
  resolved : (Int64.t, (string * int) option) Hashtbl.t;
  resolved_fun : (Int64.t, string option) Hashtbl.t;
  mutable resolved_fun_intervals : string Intervals.t;
  mutable hits : int;
  mutable misses : int;
  mutable fun_hits : int;
  mutable fun_misses : int;
  mutable intervals_hits : int;
  mutable intervals_misses : int;
  inverse : (string, (int, Int64.t) Hashtbl.t) Hashtbl.t;
}

let verbose = ref true

let create ~elf_executable =
  let fd = Unix.openfile elf_executable [ Unix.O_RDONLY ] 0 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let map =
    Bigarray.array1_of_genarray
      (Unix.map_file fd Bigarray.int8_unsigned Bigarray.c_layout false
         [| len |])
  in
  Unix.close fd;
  let _header, sections = Owee_elf.read_elf map in
  let resolved = Hashtbl.create 42 in
  let resolved_fun = Hashtbl.create 42 in
  let inverse = Hashtbl.create 42 in
  let resolved_fun_intervals = Intervals.empty in
  let strtab = Owee_elf.find_string_table map sections in
  let symtab = Owee_elf.find_symbol_table map sections in
  match (symtab, strtab) with
  | None, _ -> failwith "Can't find symbol table in elf binary"
  | _, None -> failwith "Can't find string table in elf binary"
  | Some symtab, Some strtab ->
      {
        map;
        sections;
        strtab;
        symtab;
        resolved;
        resolved_fun;
        inverse;
        hits = 0;
        misses = 0;
        fun_hits = 0;
        fun_misses = 0;
        resolved_fun_intervals;
        intervals_hits = 0;
        intervals_misses = 0;
      }

(* CR-soon mshinwell: tidy all this up. Also, the pinpointing of which row
   is the correct one isn't great. *)

type l = {
  filename : string option;
  state : Owee_debug_line.state;
}

let resolve_from_dwarf t ~f =
  match Owee_elf.find_section t.sections ".debug_line" with
  | None -> ()
  | Some section ->
      if !verbose then
        Printf.printf
          "Found section  name=%s addr=0x%Lx offset=0x%Lx size=0x%Lx\n"
          section.sh_name_str section.sh_addr section.sh_offset
          section.sh_size;
      let body = Owee_buf.cursor (Owee_elf.section_body t.map section) in
      let rec aux () =
        (* Within one chunk (aka sequence in dwarf line programs), the
           entries are traversed in the increasing order of code addresses
           their debug info refers to. There is no such guarantee between
           chunks. *)
        match Owee_debug_line.read_chunk body with
        | None -> ()
        | Some (header, chunk) ->
            let check header (state : Owee_debug_line.state)
                (prev : l option) : l option =
              if state.end_sequence then None
              else
                let filename = Owee_debug_line.get_filename header state in
                let cur =
                  { filename; state = Owee_debug_line.copy state }
                in
                f cur prev;
                Some cur
            in
            ignore
              ( Owee_debug_line.fold_rows (header, chunk) check None
                : l option );
            aux ()
      in
      aux ()

let reset_cache t =
  let report msg h m =
    let hits = float_of_int h in
    let misses = float_of_int m in
    let ratio =
      if misses +. hits > 0. then hits /. (misses +. hits) else 0.
    in
    Printf.printf "Cache %s: hit=%d, miss=%d, hit/(miss+hit)=%.3f\n" msg h m
      ratio
  in
  if !verbose then (
    report "loc" t.hits t.misses;
    report "fun" t.fun_hits t.fun_misses );
  Hashtbl.clear t.resolved;
  t.hits <- 0;
  t.misses <- 0;
  Hashtbl.clear t.inverse

let print cur _ =
  match cur.filename with
  | None -> ()
  | Some filename ->
      Printf.printf "%s\t%d\t0x%Lx\n" filename cur.state.line
        cur.state.address

let print_dwarf t = resolve_from_dwarf t ~f:print

exception FoundLoc of string * int

exception FinishedFunc

let find_range t ~start ~finish ~fill_gaps ~with_inverse cur prev =
  match prev with
  | None -> ()
  | Some prev ->
      if !verbose then (
        Printf.printf "find_range prev.state.filename=%s .addr=0x%Lx\n"
          prev.state.filename prev.state.address;
        Printf.printf "find_range cur.state.filename=%s .addr=0x%Lx\n"
          cur.state.filename cur.state.address );
      let filename =
        match prev.filename with
        | None ->
            if !verbose then
              Printf.printf
                "find_range at prev=0x%Lx: prev.filename=None, use \
                 prev.state.filename=%s\n"
                prev.state.address prev.state.filename;
            prev.state.filename
        | Some filename ->
            if !verbose then
              Printf.printf "find_range filename=%s\n" filename;
            filename
      in
      assert (prev.state.address <= cur.state.address);
      if start <= prev.state.address && finish >= prev.state.address then (
        let line = prev.state.line in
        if !verbose then
          Printf.printf "Caching loc 0x%Lx %s %d\n" prev.state.address
            filename line;
        let res = Some (filename, line) in
        ( if with_inverse then
          let tbl =
            match Hashtbl.find_opt t.inverse filename with
            | None ->
                if !verbose then
                  Printf.printf "creating a new func table for %s %d\n"
                    filename line;
                let tbl = Hashtbl.create 42 in
                Hashtbl.add t.inverse filename tbl;
                tbl
            | Some tbl ->
                if !verbose then
                  Printf.printf
                    "found func table with %d entries for %s %d\n"
                    (Hashtbl.length tbl) filename line;
                tbl
          in
          match Hashtbl.find_opt tbl line with
          | None ->
              if !verbose then
                Printf.printf
                  "adding new entry to line table for %s %d: 0x%Lx\n"
                  filename line prev.state.address;
              Hashtbl.add tbl line prev.state.address
          | Some addr ->
              if !verbose then
                Printf.printf "found entry in line table for %s %d: 0x%Lx\n"
                  filename line addr );
        if fill_gaps then
          let n =
            if cur.state.address < finish then cur.state.address else finish
          in
          let i = ref prev.state.address in
          while !i < n do
            Hashtbl.add t.resolved !i res;
            i := Int64.add !i 1L
          done
        else Hashtbl.add t.resolved prev.state.address res )

let find_offsets t ~start ~finish ~addresses cur prev =
  match prev with
  | None -> ()
  | Some prev ->
      if finish <= prev.state.address then raise FinishedFunc;
      if start <= prev.state.address then (
        let filename =
          match prev.filename with
          | None ->
              if !verbose then Printf.printf "find_offsets filename=None\n";
              prev.state.filename
          | Some filename ->
              if !verbose then
                Printf.printf "find_offsets filename=%s\n" filename;
              filename
        in
        if !verbose then
          Printf.printf "Caching loc 0x%Lx %s %d\n" prev.state.address
            filename prev.state.line;
        let res = Some (filename, prev.state.line) in
        let n =
          if cur.state.address < finish then cur.state.address else finish
        in
        List.iter
          (fun address ->
            if address <= prev.state.address && address < n then
              Hashtbl.add t.resolved address res)
          addresses )

let _resolve_function t ~sym =
  (* find function addresses *)
  let start = Owee_elf.Symbol_table.Symbol.value sym in
  let size = Owee_elf.Symbol_table.Symbol.size_in_bytes sym in
  let finish = Int64.add start size in
  if !verbose then
    Printf.printf "Resolving function for cache: (0x%Lx,0x%Lx,0x%Lx)\n"
      start size finish;

  (* CR-soon gyorsh: consider turning off fill_gaps for functions that are
     very long or do not have any linear ids. *)
  (* find dwarf locations for this function *)
  try
    resolve_from_dwarf t
      ~f:(find_range t ~start ~finish ~fill_gaps:true ~with_inverse:false)
  with FinishedFunc -> ()

let resolve_offsets t ~sym offsets =
  (* find function addresses *)
  let start = Owee_elf.Symbol_table.Symbol.value sym in
  let size = Owee_elf.Symbol_table.Symbol.size_in_bytes sym in
  let finish = Int64.add start size in
  let addresses =
    List.map (fun i -> Int64.add start (Int64.of_int i)) offsets
  in
  if !verbose then
    Printf.printf
      "Resolving function offsets for cache: (0x%Lx,0x%Lx,0x%Lx)\n" start
      size finish;

  (* find dwarf locations for this function *)
  try resolve_from_dwarf t ~f:(find_offsets t ~start ~finish ~addresses)
  with FinishedFunc -> ()

let find ~program_counter cur prev =
  match prev with
  | None -> ()
  | Some prev ->
      if
        program_counter >= prev.state.address
        && program_counter < cur.state.address
      then (
        if !verbose then (
          Printf.printf "find prev.state.filename=%s .addr=0x%Lx\n"
            prev.state.filename prev.state.address;
          Printf.printf "find cur.state.filename=%s .addr=0x%Lx\n"
            cur.state.filename cur.state.address );
        match prev.filename with
        | None ->
            if !verbose then Printf.printf "find filename=None\n";
            raise (FoundLoc (prev.state.filename, prev.state.line))
        | Some filename ->
            if !verbose then Printf.printf "find filename=%s\n" filename;
            raise (FoundLoc (filename, prev.state.line)) )

let find_all ~t ~addresses cur prev =
  match prev with
  | None -> ()
  | Some prev ->
      if !verbose then (
        Printf.printf "find_all prev.state.filename=%s .addr=0x%Lx\n"
          prev.state.filename prev.state.address;
        Printf.printf "find_all cur.state.filename=%s .addr=0x%Lx\n"
          cur.state.filename cur.state.address );
      let filename =
        match prev.filename with
        | None ->
            if !verbose then
              Printf.printf
                "find_all at prev=0x%Lx: filename=None, use prev %s\n"
                prev.state.address prev.state.filename;
            prev.state.filename
        | Some filename -> filename
      in
      if !verbose then
        Printf.printf "find_all at 0x%Lx: using filename=%s\n"
          prev.state.address filename;

      (* Find all [a] in [addresses] such that [a] is between address of
         [prev] and [cur], and cache this loc info. *)
      assert (prev.state.address <= cur.state.address);

      (* for pc = prev.state.address to cur.state.address *)
      (* but we can't use "for" because pc is int64 *)
      let rec loop pc =
        if Int64.compare pc cur.state.address < 0 then (
          ( match Hashtbl.find_opt addresses pc with
          | None ->
              if !verbose then Printf.printf "find_all: ignored 0x%Lx\n" pc;
              ()
          | Some _ ->
              if !verbose then Printf.printf "find_all: resolved 0x%Lx\n" pc;
              Hashtbl.add t.resolved pc (Some (filename, prev.state.line))
          );
          loop (Int64.add pc 1L) )
      in
      loop prev.state.address

let resolve_all t addresses =
  let len = Hashtbl.length addresses in
  if len > 0 then (
    if !verbose then
      Printf.printf "resolve_all: input size=%d unique addresses\n" len;
    Hashtbl.filter_map_inplace
      (fun k d -> if Hashtbl.mem t.resolved k then None else Some d)
      addresses;
    let l = Hashtbl.length addresses in
    if !verbose && l < len then
      Printf.printf "resolve_all: input size=%d unresolved addresses\n" l;
    let start = Hashtbl.length t.resolved in
    resolve_from_dwarf t ~f:(find_all ~t ~addresses);
    let stop = Hashtbl.length t.resolved in
    let n = stop - start in
    assert (n <= len);
    if !verbose then (
      Printf.printf "resolve_all: resolved %d addresses\n" n;
      Printf.printf "resolve_all: not resolved %d addresses\n" (len - n) ) )

let resolve_pc t ~program_counter =
  try
    resolve_from_dwarf t ~f:(find ~program_counter);
    Hashtbl.add t.resolved program_counter None;
    None
  with FoundLoc (filename, line) ->
    let result = Some (filename, line) in
    if !verbose then
      Printf.printf "Caching loc 0x%Lx %s %d\n" program_counter filename
        line;
    Hashtbl.add t.resolved program_counter result;
    result

let resolve_from_cache t ~program_counter =
  match Hashtbl.find t.resolved program_counter with
  | resolved ->
      t.hits <- t.hits + 1;
      if !verbose then
        Printf.printf "Found loc in cache 0x%Lx\n" program_counter;
      resolved
  | exception Not_found ->
      t.misses <- t.misses + 1;
      if !verbose then
        Printf.printf "Cannot resolve from cache 0x%Lx\n" program_counter;
      None

let resolve t ~program_counter =
  match Hashtbl.find t.resolved program_counter with
  | resolved ->
      t.hits <- t.hits + 1;
      if !verbose then
        Printf.printf "Found loc in cache 0x%Lx\n" program_counter;
      resolved
  | exception Not_found ->
      t.misses <- t.misses + 1;
      if !verbose then Printf.printf "Caching loc 0x%Lx\n" program_counter;
      resolve_pc t ~program_counter

let function_at_pc t ~program_counter:address =
  match
    Owee_elf.Symbol_table.functions_enclosing_address t.symtab ~address
  with
  | [] -> None
  (* Just take the first one for the moment. There will usually be only one. *)
  | sym :: _ -> Owee_elf.Symbol_table.Symbol.name sym t.strtab

let report msg name program_counter =
  if !verbose then
    Printf.printf "%s 0x%Lx:%s\n" msg program_counter
      ( match name with
      | None -> "none"
      | Some n -> n )

let resolve_function_containing t ~program_counter =
  report "Resolve_function_containing:" None program_counter;
  match Hashtbl.find t.resolved_fun program_counter with
  | name ->
      t.fun_hits <- t.fun_hits + 1;
      report "Found fun in cache" name program_counter;

      (* Only cache in resolved_fun the pcs for which no name was found. The
         others have an entry in resolved_fun_cache cache. It takes longer
         to extract but uses less memory. *)
      assert (name = None);
      None
  | exception Not_found -> (
      t.fun_misses <- t.fun_misses + 1;
      match
        Intervals.enclosing t.resolved_fun_intervals program_counter
      with
      | Some fun_interval ->
          t.intervals_hits <- t.intervals_hits + 1;
          report "Found fun in interval cache" (Some fun_interval.v)
            program_counter;
          Some fun_interval
      | None ->
          t.intervals_misses <- t.intervals_misses + 1;
          let syms =
            Owee_elf.Symbol_table.functions_enclosing_address t.symtab
              ~address:program_counter
          in
          let rec find_func syms =
            match syms with
            | [] ->
                report "Caching fun " None program_counter;
                Hashtbl.add t.resolved_fun program_counter None;
                None
            | sym :: tail ->
                let start = Owee_elf.Symbol_table.Symbol.value sym in
                let size = Owee_elf.Symbol_table.Symbol.size_in_bytes sym in
                let finish = Int64.add start size in
                if !verbose then
                  Printf.printf
                    "Find func sym: start=0x%Lx finish=0x%Lx pc=0x%Lx\n"
                    start finish program_counter;

                (* Look for symbol that contains program counter. This is
                   needed because functions_enclosing_address is based on
                   start+size of symbols, and sometimes previous symbol's
                   end of interval covers the start of the next symbol. This
                   may be a bug in Owee, or maybe intentional, but we can
                   work around it here. *)
                if start = program_counter && size = 0L then (
                  if !verbose then
                    Printf.printf
                      "Enclosing function is of size 0: start=0x%Lx \
                       finish=0x%Lx pc=0x%Lx\n"
                      start finish program_counter;
                  find_func tail )
                else if
                  (size > 0L && start = program_counter)
                  || (* size is sometimes 0 even when the function is
                        non-empty *)
                     (start < program_counter && program_counter < finish)
                  (* if size is non-zero, sometimes finish overlaps with the
                     start of the next function, so we check that the
                     program counter is strictly smaller. *)
                then (
                  match Owee_elf.Symbol_table.Symbol.name sym t.strtab with
                  | None ->
                      if !verbose then
                        Printf.printf "Symbol without a name at 0x%Lx\n"
                          start;
                      report "Caching fun " None program_counter;
                      Hashtbl.add t.resolved_fun program_counter None;
                      None
                  | Some name ->
                      let open Intervals in
                      let fun_interval =
                        { l = start; r = finish; v = name }
                      in
                      report "Caching fun " (Some name) program_counter;

                      (* Hashtbl.add t.resolved_fun program_counter (Some
                         name); *)
                      t.resolved_fun_intervals <-
                        Intervals.insert t.resolved_fun_intervals
                          fun_interval;
                      Some fun_interval )
                else find_func tail
          in
          find_func syms )

(* if the function is found and [reset] is true, then resets caches *)
let resolve_function_starting_at t ~program_counter ~reset =
  report "Resolve_function_starting_at_pc:" None program_counter;
  match Hashtbl.find t.resolved_fun program_counter with
  | name ->
      t.fun_hits <- t.fun_hits + 1;
      report "Found fun in cache" name program_counter;
      name
  | exception Not_found ->
      t.fun_misses <- t.fun_misses + 1;
      let syms =
        Owee_elf.Symbol_table.functions_enclosing_address t.symtab
          ~address:program_counter
      in
      let rec find_func syms =
        match syms with
        | [] -> None
        | sym :: tail ->
            let start = Owee_elf.Symbol_table.Symbol.value sym in
            if !verbose then
              Printf.printf "Find func sym: start=0x%Lx pc=0x%Lx\n" start
                program_counter;

            (* Look for symbol whose start address is program counter. This
               is needed because functions_enclosing_address is based on
               start+size of symbols, and sometimes previous symbol's end of
               interval covers the start of the next symbol. This may be a
               bug in Owee, or maybe intentional, but we can work around it
               here. *)
            if start = program_counter then (
              (* Once we have completed processing a function, we never go
                 back to its addresses again. *)
              if reset then reset_cache t;
              Owee_elf.Symbol_table.Symbol.name sym t.strtab )
            else find_func tail
      in
      let name = find_func syms in
      report "Caching fun " name program_counter;
      Hashtbl.add t.resolved_fun program_counter name;
      name

let _resolve_function_offsets t ~program_counter offsets ~reset =
  if !verbose then
    Printf.printf "Resolve function offsets, start=0x%Lx\n" program_counter;
  let syms =
    Owee_elf.Symbol_table.functions_enclosing_address t.symtab
      ~address:program_counter
  in
  let rec find_func syms =
    match syms with
    | [] -> None
    | sym :: tail ->
        let start = Owee_elf.Symbol_table.Symbol.value sym in
        if !verbose then
          Printf.printf "Find func sym: start=0x%Lx pc=0x%Lx\n" start
            program_counter;

        (* Look for symbol whose start address is program counter. This is
           needed because functions_enclosing_address is based on start+size
           of symbols, and sometimes previous symbol's end of interval
           covers the start of the next symbol. This may be a bug in Owee,
           or maybe intentional, but we can work around it here. *)
        if start = program_counter then (
          (* Once we have completed processing a function, we never go back
             to its addresses again. *)
          if reset then reset_cache t;
          resolve_offsets t ~sym offsets;
          Owee_elf.Symbol_table.Symbol.name sym t.strtab )
        else find_func tail
  in
  let name = find_func syms in
  name

let resolve_range t ~start ~finish ~with_inverse =
  (* find function addresses *)
  assert (start < finish);
  if !verbose then
    Printf.printf "Resolving function for cache: (0x%Lx,0x%Lx)%s\n" start
      finish
      (if with_inverse then " with inverse" else "");
  let oldlen = Hashtbl.length t.inverse in
  resolve_from_dwarf t
    ~f:(find_range t ~start ~finish ~fill_gaps:false ~with_inverse);
  if !verbose then
    Printf.printf "inverse entries before=%d after=%d\n" oldlen
      (Hashtbl.length t.inverse)

let to_address t file line =
  if !verbose then Printf.printf "to_address of %s:%d" file line;
  match Hashtbl.find_opt t.inverse file with
  | None ->
      if !verbose then Printf.printf " function not found\n";
      None
  | Some tbl ->
      let addr = Hashtbl.find_opt tbl line in
      ( if !verbose then
        match addr with
        | None -> Printf.printf " line not found\n"
        | Some addr -> Printf.printf " addr 0x%Lx\n" addr );
      addr

let find_functions t functions =
  Owee_elf.Symbol_table.iter t.symtab ~f:(fun sym ->
      match Owee_elf.Symbol_table.Symbol.type_attribute sym with
      | Func -> (
          match Owee_elf.Symbol_table.Symbol.name sym t.strtab with
          | None -> ()
          | Some name -> (
              match Hashtbl.mem functions name with
              | true -> (
                  (* bound to None *)
                  match Hashtbl.find functions name with
                  | None ->
                      let start = Owee_elf.Symbol_table.Symbol.value sym in
                      Hashtbl.replace functions name (Some start)
                  | Some _ ->
                      if !verbose then
                        Printf.printf
                          "find_functions surprised to see again  %s\n" name
                  )
              | false -> () ) )
      | _ -> ())

let iter_symbols t ~f =
  Owee_elf.Symbol_table.iter t.symtab ~f:(fun s ->
      let open Owee_elf.Symbol_table.Symbol in
      match name s t.strtab with
      | None -> ()
      | Some sn -> f sn)
