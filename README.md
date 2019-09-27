# OCamlFDO: Feedback-Directed Optimizer for OCaml

OCamlFDO is a tool for Feedback-Directed Optimization (FDO) of OCaml
programs.  It takes as input execution profiles of OCaml native code
collected with Linux Perf.  The profiles are used at compile time to
guide optimization decisions.  The current version supports only code
layout optimizations. Currently, the only implemented optimizations
are reordering of functions and basic blocks within a function.


## Installation using OPAM


Currently, [ocamlfdo](https://github.com/gretay-js/ocamlfdo) is not
yet available from opam repositry.  It requires a few small changes to
its dependencies:
- [ocamlopt](https://github.com/ocaml/ocaml) compiler.
  The changes are [available here](https://github.com/gretay-js/ocaml/tree/fdo408)
  on a branch off of a 4.08 version of the compiler. Corresponding PRs
  are being reviewed upstream.
- [owee](https://github.com/let-def/owee) library.
  The change is to use int64 instead of int for addresses,
  and there are a few minor improvements.
- [ocamlcfg](https://github/gretay-js/ocamlcfg) library is not yet
  available from opam repository.


For now, the process is:
```
$ opam create fdo408 --empty
$ opam pin ocaml-variants https://github.com/gretay-js/ocaml.git#fdo408
$ opam pin ocamlcfg https://github.com/gretay-js/ocaml.git
$ opam pin add ocamlfdo https://github.com/gretay-js/ocamlfdo
```

The goal is to simplify it to:
```
$ opam install ocamlfdo
```

## Simple example: walk through

* Build 1
  ```
  $ ocamlfdo compile -extra-debug -- test2.ml -o test2.exe
  ```

    Build the executable you intend to profile and optimize. Here
    `ocamlfdo compile` acts as a wrapper of ocamlopt, and passes all
    the arguments after `--` to ocamlopt.  This will produce an
    executable that, compared to a normal non-FDO build, differs only
    in additional debug information.

* Profile

    ```
    perf record -e cycles:u -j any,u -o test2.perf.data ./test2.exe
    ```
    Use `Linux perf` to collect execution counters.
    It requires hardware support for performance monitoring, in particular
    the last branch record (LBR) support.
    You should run this command on a benchmarking machine.

    The ocamlfdo tool will generate two files alongside the executable: a
    linker script `test2.exe.linker-script-hot` and a profile
    `test2.exe.fdo-profile`.

* Decode
  ```
  ocamlfdo decode -binary test2.exe -perf-profile test2.perf.data
  ```
  Aggregate the samples collected by `perf` and map them to program locations,
  using extra debug information.
  After this, the original executable and perf.data files are no longer needed.

  It will produce two files:
  - `test2.exe.fdo-profile` is an execution profile of the program.
    It is used for basic block reordering.
  - `test2.exe.linker-script-hot` contains layout of hot functions,
    in a format suitable for inclusion in a linker script used by `GNU ld` linker.
    It is used for function reordering.

  The profile and the hot function layout files can be added to source
  countrol.  They should be much smaller in size than the original
  executable and perf output files.

    A complete linker script can be generated from a template:
    ```
    ocamlfdo linker-script -linker-script-hot test2.exe.linker-script-hot -o linker-script
    ```

    This command uses the [default](resources/linker-script) template provided with
    `ocamlfdo`. An alternative template can be specified on command
    line using `-linker-script-template` option.

    If the linker runs from the directory where linker-script-hot is
    located, there is no need for `ocamlfdo linker-script` command.
    Pass `linker-script-template` instead of `linker-script` to the
    compiler in the next step, and the linker will find
    `linker-script-hot` automatically.

* Build 2

   ```
    $ ocamlfdo compile -fdo-profile test2.exe.fdo-profile -reorder-blocks opt -- \
      examples/simple/test2.ml -o examples/simple/test2.exe \
      -function-sections -ccopt -Wl,--script=linker-script
  ```
  This produces optimized executable.


## How to use ocamlfdo in a project?

In a project that has multiple files and libraries, the easiest way at
the moment is (sadly) to trick the build system into thinking that
`ocamlfdo compile` is ocamlopt.

A quick hack is a symbolic link to [ocamlfdo.sh](examples/ocamlfdo.sh) script.

```
$ cat > ocamlfdo.sh << EOF
#!/bin/sh
set -x -eu pipefail
ocamlfdo compile -auto -fdo-profile $FDO_PROFILE -md5-unit -- "$@" -function-sections --ccopt -Wl,--script=$LINKER_SCRIPT
EOF
$ ln -s ocamlfdo.sh `which ocamlopt.opt`
$ chmod +x ocamlfdo.sh
$ export FDO_PROFILE=test2.exe.fdo-profile
$ export LINKER_SCRIPT=linker-script-template
$ touch test2.exe.linker-script-hot
```

Then, the simple example can be simplified a little more:
```
$ ocamlopt test2.ml -o test2.exe
$ perf record -e cycles:u -j any,u ./test2.exe
$ ocamlfdo decode -binary test2.exe -perf-profile perf.data
$ ocamlopt test2.ml -o test2.exe
```

The nice thing is that now when dune (or make or another build system)
invoke ocamlopt via
ocamlfdo wrapper, it will use the profile whenever it can.

The script `ocamlfdo.sh` invokes `ocamlfdo compile` with `-auto` argument.
If the profile file referred to in -fdo-profile exists,
it will be used; otherwise `-extra-debug` is added instead.

When the project relies on packages installed via opam in the current switch,
these libraries may need to be rebuilt (both build 1 and 2) to take
full advantage of profile information for the project.
There are at least two hacky ways of achieving it, and no decent ones yet.
- force opam to reinstall all the packages in each build, with
  ocamlfdo now mascarading as ocamlopt.
- use [duniverse](https://github.com/avsm/duniverse)


