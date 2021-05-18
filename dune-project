(lang dune 1.10)
(using fmt 1.1)
(name ocamlfdo)
(version 0.1.2)

(license MIT)
(maintainers "Greta Yorsh <gyorsh@janestreet.com>")
(authors "Greta Yorsh <gyorsh@janestreet.com>")
(source (github gretay-js/ocamlfdo))

(package
 (name ocamlfdo)
 (synopsis "Feedback-Directed Optimizer for OCaml")
 (description "
OCamlFDO is a tool for Feedback-Directed Optimization (FDO) of OCaml programs.
It takes as input execution profiles of OCaml native code collected with Linux Perf.
The profiles are used at compile time to guide optimization decisions.
The current version supports only code layout optimizations.

Copyright (c) 2019 Jane Street Group LLC.
")

 (depends
  (ocaml (>= 4.11))
  (dune (>= 1.8))
  (ocamlcfg (>= 0.1.0))
  owee
  (core (>= 0.14.0))
  ppx_jane
  dune-build-info
  re2
  (shexp  (>= v0.14.0))
  shexp
  parsexp_io
 )
)

(generate_opam_files true)
