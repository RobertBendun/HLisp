hlisp: hlisp.hs Evaluator.hs Assembler.hs Parser.hs
	ghc hlisp.hs -j -hidir build -odir build -O2
