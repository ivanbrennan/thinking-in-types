.PHONY: default
default:
	@nix-shell --pure --run "ghcid '--command=ghci Exercises.hs'"

.PHONY: lint
lint:
	@nix-shell --pure --run "hlint ."
