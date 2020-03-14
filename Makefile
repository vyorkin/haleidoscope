dev:
	stack build --test --no-run-tests
	ghcid --restart=src/Sandbox/Calc/Parser.y --command="cabal repl" | source-highlight -s haskell -f esc
repl:
	cabal repl
build:
	cabal build
clean:
	cabal clean
prof:
	cabal configure --enable-profiling
noprof:
	cabal configure --disable-profiling

.PHONY: dev repl build clean prof noprof
