#!/bin/bash

jtw opam stringext -o html/_opam
dune exec -- odoc_notebook generate guided_tour.md --odoc-dir _tmp/_odoc

