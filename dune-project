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
 (synopsis "Monkey Language but in OCaml")
 (description "Monkey Language but in OCaml")
 (tags
  (monkey interpreter compiler bytecode "virtual machine"))
 (depends ocaml ppx_deriving ppx_inline_test))