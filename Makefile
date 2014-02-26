all: penny.cabal

penny.cabal: penny.cabal.m4 versions.m4
	m4 versions.m4 penny.cabal.m4 > penny.cabal

clean:
	rm -f penny.cabal

minimum-versions.txt : penny.cabal sunlight-test.hs
	runghc sunlight-test.hs

.PHONY: clean
