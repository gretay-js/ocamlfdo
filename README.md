# OCamlFDO: Feedback-Directed Optimizer for OCaml

OCamlFDO is a tool for Feedback-Directed Optimization (FDO) of OCaml
programs.  It takes as input execution profiles of OCaml native code
collected with Linux Perf.  The profiles are used at compile time to
guide optimization decisions.  The current version supports only code
layout optimizations. Currently, the only implemented optimizations
are reordering of functions and basic blocks within a function.


## Installation using OPAM


Currently, [ocamlfdo](https://github.com/gretay-js/ocamlfdo) is not
yet available from opam repository.  It requires a few small changes to
the upstream version of its dependencies:
- [ocamlopt](https://github.com/ocaml/ocaml) compiler.  The changes
  are [available here](https://github.com/gretay-js/ocaml/tree/fdo408)
  on a branch off of a 4.08 version of the compiler. Corresponding PRs
  are being reviewed upstream.
- [owee](https://github.com/let-def/owee) library.
  The change is to use int64 instead of int for addresses,
  and there are a few minor improvements.
- [ocamlcfg](https://github.com/gretay-js/ocamlcfg) library is not yet
  available from opam repository.
- [ocaml-migrate-parsetree](https://github.com/ocaml-ppx/ocaml-migrate-parsetree)
  library.
  To build ocamlfdo using the compiler branch above,
  which is based on janestreet compiler,
  we need to update ast in ocaml-migrate-parsetree to handle immediate64
  (an unrelated compiler change that will be included in 4.10).

For now, the process is:
```
opam switch create fdo408 --empty
opam pin add ocaml-variants https://github.com/gretay-js/ocaml.git#fdo408
opam pin add ocaml-migrate-parsetree https://github.com/gretay-js/ocaml-migrate-parsetree.git#409
opam pin add ocamlcfg https://github.com/gretay-js/ocamlcfg.git
opam pin add owee https://github.com/gretay-js/owee.git#wip3
opam install ppx_csv_conv ppx_variants_conv parsexp_io shexp re2
opam pin add ocamlfdo https://github.com/gretay-js/ocamlfdo.git
```

Assuming that all the changes are accepted upstream, the installation
process will be simply:
```
opam install ocamlfdo
```

## Simple example: walk through

* Build 1
    ```
    ocamlfdo compile -extra-debug -- test2.ml -o test2.exe
    ```
    Build the executable you intend to profile and optimize. Here
    `ocamlfdo compile` acts as a wrapper of `ocamlopt`, and passes all
    the arguments after `--` to `ocamlopt`.  This will produce an
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

* Decode
  ```
  ocamlfdo decode -binary test2.exe -perf-profile test2.perf.data
  ```
  Aggregate the samples collected by `perf` and map them to program locations,
  using extra debug information.
  After this, the original executable and perf.data files are no longer needed.
  It will produce `test2.exe.fdo-profile` file that contains
  an execution profile of the program. This profile is used for reordering
  functions (via a linker script) and basic blocks.
  The profile file  can be added to source
  countrol.  It should be much smaller in size than the original
  executable and perf output files.

  A complete linker script can be generated from a template as follows:
  ```
  ocamlfdo linker-script -fdo-profile test2.exe.fdo-profile -o linker-script
  ```

  This command uses the [default](resources/linker-script) template provided with
  `ocamlfdo`. An alternative template can be specified on command
  line using `-linker-script-template` option.

* Build 2

   ```
   ocamlfdo compile -fdo-profile test2.exe.fdo-profile -reorder-blocks opt -- \
      test2.ml -o test2.fdo.exe \
      -function-sections -ccopt -Wl,--script=linker-script
  ```
  This produces optimized executable.


##  Advanced use of linker scripts and linking on non-GNU systems

Invoking `ocamlfdo decode` with the option `-write-linker-script-hot`
will also save a layout of hot functions to a separate file,
in a format suitable for inclusion in a linker script used by `GNU
ld` linker. Users can inspect and modify this layout for experimenting, but be aware
that function symbol names may change even when source code of the
function has not changed.
To produce a complete linker script with this layout, run
```
ocamlfdo linker-script -linker-script-hot test2.exe.linker-script-hot -o linker-script
```
If the linker runs from the directory where linker-script-hot is
located, there is no need for `ocamlfdo linker-script` command.
Pass `linker-script-template` instead of `linker-script` to the
compiler in build 2, and the linker will find
`linker-script-hot` automatically.

## Integration with dune

A new version of dune, with [support for
FDO](https://github/gretay-js/dune) is under review.
```
opam pin add dune https://github.com/gretay-js/dune.git#fdo-decode
```

To enable an FDO build, create a new context in your `dune-workspace`
and specify the target executable to optimize.
```
(context (default
           (fdo src/foo.exe)
           ))
```

Dune will look for a profile file named `src/foo.exe.fdo-profile` in
the source directory. If the profile file does not exists, dune will
perform build 1, otherwise it will use the profile to perform build 2.
Users should invoke `ocamlfdo decode` to generate the profile from
`perf.data`.

The advantage of integrating `ocamlfdo` with `dune` is that it avoids
most of recompilation when the profile changes, making build 2 much
faster than build 1. Essentially, only the last step of emitting
assembly needs to be done in build 2. It is achieved using split
compilation.

### Experimental

Instead of manually invoking `ocamlfdo decode`, users can invoke `dune
build @fdo-decode` to generate a new profile from
`src/foo.exe.perf.data` found in the source directory.  It will use
`src/foo.exe` executable found in the build directory, or build one
afresh from source (using the previous profile, if available).  User
can do `dune promote` to copy the new profile to the source
directory. Caution!! You must rename or remove `src/foo.exe.perf.data`
immediate after `dune promote` to avoid corrupting the profile in the
next invocation of `dune`.

The next inocation of `build @fdo-decode` will use the new profile to
build a new executable, and then will try to use the new executable to
decode the old `perf.data`.  It will silently produce a new and bogous
profile, that should not be promoted. Note that `ocamlfdo` will discover
that the profile is bogous only after the profile is promoted and
another build tries to use it.


## Optimizing external dependencies with FDO

When the project relies on packages installed via opam in the current switch,
these libraries may need to be rebuilt (both build 1 and 2) to take
full advantage of profile information for the project.

There are at least two hackish ways of achieving it.
- force opam to reinstall all the packages in each build, with
  ocamlfdo now mascarading as ocamlopt.
  ```
  dune external-lib-deps @@default | sed 's/^- //' | xargs
  ```
  prints the list of libraries my project depends on, which almost
  gives the list of packages to reinstall.
  ```
  opam reinstall <list of packages my project depends on> --forget-pending
  ```
  Then, re-link of the final executable.

- use [duniverse](https://github.com/avsm/duniverse)
  ```
  opam pin https://github.com/avsm/duniverse.git
  duniverse init ??
  duniverse pull ??
  ```

## Split compilation

Under the hood, `ocamlfdo compile` splits the standard
compilation into two phases: `compile` and `emit`.  The
`compile` phase stops `ocamlopt` after scheduling pass in the
backend, and saves the intermediate representation into a file. The
`emit` phase reads that file and continues to emit assembly,
assemble, link, etc, depending on the options passed to `ocamlopt`.

In an FDO build, after `compile` phase, the intermediate
representation is optimized using the profile, and then passed on to
the `emit` phase.  If the profile changes, there is no need to repeat
`compile` phase to rebuild the target executable. Dune rules for FDO
do just that, instead of invoking `ocamlfdo compile` directly.

In summary, `ocamlfdo compile` is composed of three phases:
- `compile` phase: `ocamlopt -stop-after scheduling -save-ir-after
scheduling ...` outputs `.cmir-linear` file and the usual compilation artifacts
including `.cmx`, but not `.S` or `.o`.
- fdo phase: `ocamlfdo opt ...` with the appropriate options for build1 or build 2,
reads `.cmir-linear` and writes `.cmir-linear-fdo`.
- `emit` phase: `ocamlopt -start-from emit ..` reads `.cmir-linear-fdo`
and produces the rest of the compilation targets and artifacts.


## How to use ocamlfdo in a project that is not built with dune?

In a project that has multiple files and libraries, the easiest way at
the moment is (sadly) to trick the build system into thinking that
`ocamlfdo compile` is ocamlopt.

A quick hack is a symbolic link to [ocamlfdo.sh](examples/ocamlfdo.sh) script.

```
$ cat > ocamlfdo.sh << EOF
#!/bin/sh
set -x -eu pipefail
ocamlfdo compile -auto $OCAMLFDO_FLAGS -- "$@" $OCAMLOPT_FLAGS_FOR_FDO
EOF
$ ln -s ocamlfdo.sh `which ocamlopt.opt`
$ chmod +x ocamlfdo.sh
```
When OCAMLFDO_FLAGS is empty, ocamlfdo.sh acts as a simple wrapper for
ocamlopt. To enable FDO, set the following environment variables:
```
$ export FDO_PROFILE=test2.exe.fdo-profile
$ export FDO_PROFILE_FLAG="-fdo-profile $FDO_PROFILE"
$ export OCAMLFDO_FLAGS="$FDO_PROFILE_FLAG -md5-unit"
$ export LINKER_SCRIPT=linker-script-template
$ export OCAMLOPT_FLAGS_FOR_FDO="-function-sections --ccopt -Wl,--script=$LINKER_SCRIPT"
```

Then, the simple example can be simplified a little more:
```
$ touch test2.exe.linker-script-hot
$ ocamlopt test2.ml -o test2.exe
$ perf record -e cycles:u -j any,u ./test2.exe
$ ocamlfdo decode -binary test2.exe -perf-profile perf.data -write-linker-script -linker-script-hot linker-script-hot
$ ocamlopt test2.ml -o test2.exe
```

The nice thing is that now a build system that invokes ocamlopt via
ocamlfdo wrapper will use the profile whenever it can.

The script `ocamlfdo.sh` invokes `ocamlfdo compile` with `-auto` argument.
If the profile file referred to in `-fdo-profile` option exists,
it will be used; otherwise `-extra-debug` is added instead.
If there is no -fdo-profile option, then `ocamlfdo compile` does not
split compilation.


