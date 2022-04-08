all:
	ghc -Wall -o ../flp21-fun -i src/*.hs --make 
clean:
	rm -rf *.hi *.o flp21-fun