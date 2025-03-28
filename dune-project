(lang dune 3.0)

(generate_opam_files)

(package
 (name hello-ao)
 (depends
  (ocaml
   (>= "5.0.0"))
  ao
  (tsdl
   (>= 1))))
