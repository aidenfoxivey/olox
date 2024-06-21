(lang dune 3.14)

(name olox)

(generate_opam_files true)

(source
 (uri https://forgejo.aidenfoxivey.com/aidenfoxivey/olox.git))

(authors "Aiden Fox Ivey")

(maintainers "Aiden Fox Ivey")

(license BSD-2-Clause)

(documentation https://forgejo.aidenfoxivey.com/aidenfoxivey/olox/wiki)

(package
 (name olox)
 (synopsis "Lox interpreter in OCaml.")
 (description "Lox interpreter based on Crafting Interpreters by Robert Nystrom.")
 (depends ocaml dune)
 (tags
  (interpreter lox)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
