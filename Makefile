all: penny.cabal

penny.cabal: penny.cabal.m4 versions.m4
	m4 versions.m4 penny.cabal.m4 > penny.cabal
