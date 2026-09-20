(lang dune 3.14)
(name stdlib_v2)
(version 0.1.0)
(generate_opam_files true)
(authors "Yotam Barnoy <yotambarnoy@gmail.com>")
(maintainers "Yotam Barnoy <yotambarnoy@gmail.com>")
(source (github bluddy/stdlib-v2))
(package
 (name stdlib_v2)
 (synopsis "Effect-polymorphic standard library overlay for OCaml with typed effects")
 (description
  "An overlay for the OCaml standard library providing effect-polymorphic higher-order functions while maintaining 100% compatibility with pure code.")
 (license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception"))
