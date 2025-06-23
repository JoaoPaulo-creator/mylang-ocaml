(lang dune 3.19)

(name mylang-ocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(license MIT)


(package
 (name mylang-ocaml)
 (synopsis "Port da linguagem criada no curso do terminalroot em ocaml")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
