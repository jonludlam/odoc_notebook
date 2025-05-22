#!/bin/bash

dune exec -- odoc_notebook opam astring base
dune exec -- odoc_notebook generate guided_tour.md --odoc-dir _tmp/_odoc

