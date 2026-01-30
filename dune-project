(lang dune 3.20)

(name advent-of-code-2025-ocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name advent-of-code-2025-ocaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   containers
   ppx_deriving
  (alcotest :with-test))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
