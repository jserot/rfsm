(lang dune 2.6)
(name rfsm)
(source (github jserot/rfsm))
(license MIT)
(authors "Jocelyn Sérot <jocelyn.serot@uca.fr>")
(maintainers "Jocelyn Sérot <jocelyn.serot@uca.fr>")
(bug_reports "jocelyn.serot@uca.fr")
(homepage "http://github.com/jserot/rfsm")
(documentation "http://jserot.github.io/rfsm")
(using menhir 2.0)
(allow_approximate_merlin)

(package
  (name rfsm)
  (synopsis "A toolset for describing and simulating StateChart-like state diagrams")
  (description "A toolset for describing and simulating StateChart-like state diagrams")
  (depends
    (menhir (>= 2.0))
    (lascar (>= 0.7))
    (ocamlgraph (>= 1.8))
    (ocaml (>= 4.10))))

(generate_opam_files true)
