.PHONY: default
default:
	@nix-shell --pure --run "ghcid '--command=ghci Exercises.hs'"

.PHONY: repl
repl:
	@nix-shell --pure --run "ghci Exercises.hs"

.PHONY: lint
lint:
	@nix-shell --pure --run "hlint ."
