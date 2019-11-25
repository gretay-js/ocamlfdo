open Core

type t = {
  map : Owee_buf.t;
  sections : Owee_elf.section array;
  strtab : Owee_elf.String_table.t;
  symtab : Owee_elf.Symbol_table.t;
  resolved : (string * int) option Hashtbl.M(Elf_addr).t;
  resolved_fun : string option Hashtbl.M(Elf_addr).t;
  mutable resolved_fun_intervals : string Intervals.t;
  mutable hits : int;
  mutable misses : int;
  mutable fun_hits : int;
  mutable fun_misses : int;
  mutable intervals_hits : int;
  mutable intervals_misses : int;
  inverse : Elf_addr.t Hashtbl.M(Int).t Hashtbl.M(String).t;
}

let verbose = ref true

let create ~elf_executable =
  let fd = Unix.openfile elf_executable ~mode:[ Unix.O_RDONLY ] ~perm:0 in
  let len = Unix.lseek fd 0L ~mode:Unix.SEEK_END |> Int64.to_int_exn in
  let map =
    Bigarray.array1_of_genarray
      (Unix.map_file fd Bigarray.int8_unsigned Bigarray.c_layout
         ~shared:false [| len |])
  in
  Unix.close fd;
  let _header, sections = Owee_elf.read_elf map in
  let resolved = Hashtbl.create (module Elf_addr) in
  let resolved_fun = Hashtbl.create (module Elf_addr) in
  let inverse = Hashtbl.create (module String) in
  let resolved_fun_intervals = Intervals.empty in
  let strtab = Owee_elf.find_string_table map sections in
  let symtab = Owee_elf.find_symbol_table map sections in
  match (symtab, strtab) with
  | None, _ -> Report.user_error "Can't find symbol table in elf binary"
  | _, None -> Report.user_error "Can't find string table in elf binary"
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
      Float.(
      if misses +. hits > 0. then hits /. (misses +. hits) else 0.)
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
      Printf.printf "%s\t%d\t0x%x\n" filename cur.state.line
        cur.state.address

let print_dwarf t = resolve_from_dwarf t ~f:print

let find_range t ~start ~finish ~fill_gaps ~with_inverse cur prev =
  match prev with
  | None -> ()
  | Some prev ->
      if !verbose then (
        Printf.printf "find_range prev.state.filename=%s .addr=0x%x\n"
          prev.state.filename prev.state.address;
        Printf.printf "find_range cur.state.filename=%s .addr=0x%x\n"
          cur.state.filename cur.state.address );
      let filename =
        match prev.filename with
        | None ->
            if !verbose then
              Printf.printf
                "find_range at prev=0x%x: prev.filename=None, use \
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
          Printf.printf "Caching loc 0x%x %s %d\n" prev.state.address
            filename line;
        let res = Some (filename, line) in
        ( if with_inverse then
          let tbl =
            match Hashtbl.find t.inverse filename with
            | None ->
                if !verbose then
                  Printf.printf "creating a new func table for %s %d\n"
                    filename line;
                let tbl = Hashtbl.create (module Int) in
                Hashtbl.set t.inverse ~key:filename ~data:tbl;
                tbl
            | Some tbl ->
                if !verbose then
                  Printf.printf
                    "found func table with %d entries for %s %d\n"
                    (Hashtbl.length tbl) filename line;
                tbl
          in
          match Hashtbl.find tbl line with
          | None ->
              if !verbose then
                Printf.printf
                  "adding new entry to line table for %s %d: 0x%x\n"
                  filename line prev.state.address;
              Hashtbl.set tbl ~key:line ~data:prev.state.address
          | Some addr ->
              if !verbose then
                Printf.printf "found entry in line table for %s %d: 0x%x\n"
                  filename line addr );
        if fill_gaps then
          let n =
            if cur.state.address < finish then cur.state.address else finish
          in
          let i = ref prev.state.address in
          while !i < n do
            Hashtbl.add_exn t.resolved ~key:!i ~data:res;
            i := Elf_addr.( + ) !i 1
          done
        else Hashtbl.add_exn t.resolved ~key:prev.state.address ~data:res )

let find_all ~t:_ ~addresses cur prev =
  match prev with
  | None -> ()
  | Some prev ->
      if !verbose then (
        Printf.printf "find_all prev.state.filename=%s .addr=0x%x\n"
          prev.state.filename prev.state.address;
        Printf.printf "find_all cur.state.filename=%s .addr=0x%x\n"
          cur.state.filename cur.state.address );
      let filename =
        match prev.filename with
        | None ->
            if !verbose then
              Printf.printf
                "find_all at prev=0x%x: filename=None, use prev %s\n"
                prev.state.address prev.state.filename;
            prev.state.filename
        | Some filename -> filename
      in
      if !verbose then
        Printf.printf "find_all at 0x%x: using filename=%s\n"
          prev.state.address filename;

      (* Find all [a] in [addresses] such that [a] is between address of
         [prev] and [cur], and cache this loc info. *)
      assert (prev.state.address <= cur.state.address);

      (* for pc = prev.state.address to cur.state.address *)
      (* but we can't use "for" because pc is int64 *)
      let rec loop pc =
        if Elf_addr.compare pc cur.state.address < 0 then (
          ( match Hashtbl.find addresses (Elf_addr.get pc) with
          | None ->
              if !verbose then Printf.printf "find_all: ignored 0x%x\n" pc;
              ()
          | Some _ ->
              if !verbose then Printf.printf "find_all: resolved 0x%x\n" pc;
              let dbg = { Dbg.file = filename; line = prev.state.line } in
              Hashtbl.set addresses ~key:(Elf_addr.get pc) ~data:(Some dbg)
          );
          loop (Elf_addr.( + ) pc 1) )
      in
      loop prev.state.address

let resolve_all t addresses =
  let len = Hashtbl.length addresses in
  if len > 0 then (
    if !verbose then
      Printf.printf "resolve_all: input size=%d unique addresses\n" len;
    resolve_from_dwarf t ~f:(find_all ~t ~addresses);
    let resolved = Hashtbl.count addresses ~f:Option.is_some in
    assert (resolved <= len);
    if !verbose then
      Printf.printf "resolve_all: resolved %d out of %d addresses\n"
        resolved len )

let report msg name program_counter =
  if !verbose then
    Printf.printf "%s 0x%Lx:%s\n" msg program_counter
      ( match name with
      | None -> "none"
      | Some n -> n )

let resolve_function_containing t ~program_counter =
  report "Resolve_function_containing:" None program_counter;
  let addr = Elf_addr.mk program_counter in
  match Hashtbl.find t.resolved_fun addr with
  | Some name ->
      t.fun_hits <- t.fun_hits + 1;
      report "Found fun in cache" name program_counter;

      (* Only cache in resolved_fun the pcs for which no name was found. The
         others have an entry in resolved_fun_cache cache. It takes longer
         to extract but uses less memory. *)
      assert (Option.is_none name);
      None
  | None -> (
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
                Hashtbl.add_exn t.resolved_fun ~key:addr ~data:None;
                None
            | sym :: tail ->
                let start = Owee_elf.Symbol_table.Symbol.value sym in
                let size = Owee_elf.Symbol_table.Symbol.size_in_bytes sym in
                let finish = Addr.( + ) start program_counter in
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
                let open Int64 in
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
                      Hashtbl.add_exn t.resolved_fun ~key:addr ~data:None;
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

let resolve_range t ~start ~finish ~with_inverse =
  (* find function addresses *)
  assert Int64.(start < finish);
  if !verbose then
    Printf.printf "Resolving function for cache: (0x%Lx,0x%Lx)%s\n" start
      finish
      (if with_inverse then " with inverse" else "");
  let oldlen = Hashtbl.length t.inverse in
  resolve_from_dwarf t
    ~f:
      (find_range t ~start:(Elf_addr.mk start) ~finish:(Elf_addr.mk finish)
         ~fill_gaps:false ~with_inverse);
  if !verbose then
    Printf.printf "inverse entries before=%d after=%d\n" oldlen
      (Hashtbl.length t.inverse)

let to_address t (dbg : Dbg.t) =
  if !verbose then Printf.printf "to_address of %s:%d" dbg.file dbg.line;
  match Hashtbl.find t.inverse dbg.file with
  | None ->
      if !verbose then Printf.printf " function not found\n";
      None
  | Some tbl ->
      let addr = Hashtbl.find tbl dbg.line in
      ( if !verbose then
        match addr with
        | None -> Printf.printf " line not found\n"
        | Some addr -> Printf.printf " addr 0x%x\n" addr );
      Option.map ~f:Elf_addr.get addr

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
                      Hashtbl.set functions ~key:name ~data:(Some start)
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
