all:
	ghc -Wall -o flp21-fun -i src/*.hs --make 
clean:
	rm -rf src/*.hi src/*.o flp21-fun