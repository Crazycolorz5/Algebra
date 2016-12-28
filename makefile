ackerman:
	ghc NonPrimitiveRecursion.hs -o Main -O2
	make clean

clean:
	rm *.hi *.o
