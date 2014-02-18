all: penny.cabal

penny.cabal: penny.cabal.m4 versions.m4
	m4 versions.m4 penny.cabal.m4 > penny.cabal

run-tests.sh: run-tests.sh.m4 versions.m4
	m4 versions.m4 run-tests.sh.m4 > run-tests.sh

tests: penny.cabal run-tests.sh
	sh run-tests.sh

clean:
	rm -f penny.cabal

.PHONY: clean tests
