(lang dune 1.11)
(using fmt 1.1)

(package
 (name ocamlfdo)
 (synopsis "Feedback-Direct Optimizer for OCaml")
 (description "OCamlFDO is a tool for Feedback-Directed Optimization (FDO)
of OCaml programs. It takes as input execution profiles of Ocaml native code collected with Linux Perf. The profiles are used at compile time to guide optimization decisions.
The current version supports only code layout optimizations."
))
