## Test basic functionality of ocamlfdo

- `dune runtest` tests the commands
  - `ocamlfdo compile`
  - `ocamlfdo linker-script`
  - `ocamlfdo opt`

- `dune build @test/runtest-decode` tests `ocamlfdo decode`.

    Requires hardware support for PMU sampling on the test machine.
    It usually won't work on a VM, for example.

    Run manually on a benchmarking machine using the command from `@test/runtest-perf` alias.
    Then copy perf.data to
    test/test2.perf.data.expected and `dune build @test/runtest-decode`.
    It requires Linux perf.

