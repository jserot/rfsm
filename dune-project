(lang dune 3.11)
(name rfsm)
(source (github jserot/rfsm))
(license MIT)
(authors "Jocelyn Sérot <jocelyn.serot@uca.fr>")
(maintainers "Jocelyn Sérot <jocelyn.serot@uca.fr>")
(bug_reports "jocelyn.serot@uca.fr")
(homepage "http://github.com/jserot/rfsm")
(documentation "http://jserot.github.io/rfsm")
(using menhir 2.0)

(package
  (name rfsm)
  (synopsis "A toolset for describing and simulating StateChart-like state diagrams")
  (depends
    (menhir (>= 2.0))
    (ocamlgraph (>= 2.0))
    (ocaml (>= 5.0))))

;(generate_opam_files true)
