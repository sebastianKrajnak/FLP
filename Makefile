all:
	ghc -Wall -o flp21-fun -i src/*.hs --make 
clean:
	rm -rf src/*.hi src/*.o flp21-fun
testi:
	./flp21-fun	-i test/test1.in
test1:
	./flp21-fun	-1 test/test1.in