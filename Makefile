output: src/main.s gfx/background.chr gfx/sprites.chr
	cl65 src/main.s --target nes --verbose -o flappy.nes
