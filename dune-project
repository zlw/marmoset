(lang dune 3.17)

(name marmoset)

(generate_opam_files true)

(source
 (github zlw/marmoset-ml))

(authors "Krzysztof Zalewski <zlw.zalewski@gmail.com>")

(maintainers "Krzysztof Zalewski <zlw.zalewski@gmail.com>")

(license MIT)

(documentation https://github.com/zlw/marmoset-ml)

(package
 (name marmoset)
 (synopsis "Statically typed language compiling to Go")
 (description "A statically typed language implemented in OCaml with Go code generation")
 (tags
  (language compiler typechecker go))
 (depends ocaml ppx_deriving ppx_inline_test (lsp (and (>= 1.17) (< 1.18))) linol linol-lwt))
