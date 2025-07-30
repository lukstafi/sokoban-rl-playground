(lang dune 3.19)

(name sokoban-rl-playground)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name sokoban-rl-playground)
 (synopsis "Sokoban environment for RL training and visualization")
 (description "A Sokoban game environment implemented in OCaml for reinforcement learning experiments, featuring tabular Q-learning and REINFORCE scaffolding with curriculum learning")
 (depends ocaml dune)
 (tags
  ("reinforcement-learning" "game" "sokoban" "ocaml")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
