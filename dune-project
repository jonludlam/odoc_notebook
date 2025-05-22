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
 (depends ocaml dune ppx_deriving_yojson cmdliner note brr js_top_worker merlin-js opam-format ocamlfind odoc-parser yojson eio logs mustache tyxml base64 imagelib rpclib js_of_ocaml-lwt eio_main js_top_worker-web odoc)
 (allow_empty)
 (tags
  (topics "to describe" your project)))

(package
 (name odoc_notebook_test)
 (synopsis "A tool for testing odoc_notebook pages")
 (description "A tool for testing odoc_notebook pages")
 (depends ocaml dune cmdliner js_top_worker js_top_worker-client)
 (allow_empty))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
