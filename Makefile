dev:
	ghcid --restart=src/Sandbox/Parsing/Lexer.x --restart=src/Sandbox/Parsing/Parser.y -c cabal repl | source-highlight -s haskell -f esc
repl:
	cabal repl
build:
	cabal build
prof:
	cabal configure --enable-profiling
noprof:
	cabal configure --disable-profiling

.PHONY: dev build prof noprof
