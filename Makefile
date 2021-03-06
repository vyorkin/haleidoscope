dev:
	stack build --test --no-run-tests
	ghcid --restart=src/Haleidoscope/Lexer.x --restart=src/Haleidoscope/Parser.y --command="cabal repl" | source-highlight -s haskell -f esc
repl:
	cabal repl
build:
	cabal build
clean:
	cabal clean
tags:
	rm -f tags codex.tags
	rm -rf .stack-work
	codex update --force
prof:
	cabal configure --enable-profiling
noprof:
	cabal configure --disable-profiling

.PHONY: dev repl build clean tags prof noprof
