(lang dune 3.13)
(using mdx 0.4)
(cram enable)

(name odoc_notebook)

(generate_opam_files true)

(source
 (github jonludlam/odoc_notebook))

(authors "Jon Ludlam")

(maintainers "Jon Ludlam")

(license ISC)

(documentation https://url/to/documentation)

(package
 (name odoc_notebook)
 (synopsis "A tool to create Jupyter-style notebooks from mld pages")
 (description "A tool to create Jupyter-style notebooks from mld pages")
 (depends ocaml dune ppx_deriving_yojson cmdliner note brr js_top_worker merlin-js opam-format findlib odoc-parser yojson eio logs mustache tyxml base64 imagelib rpclib)
 (allow_empty)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
