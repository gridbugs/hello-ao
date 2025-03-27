(lang dune 3.17)

(generate_opam_files)

(package
 (name hello-ao)
 (depends
  (ocaml
   (>= "5.0.0"))
  ao
  (tsdl
   (>= 1))))
