# Simplify context-free grammar

Program which applies algorithm 4.3 from TIN class ([source](http://www.fit.vutbr.cz/study/courses/TIN/public/Texty/TIN-studijni-text.pdf "Source")) to remove useless symbols, works in two steps depeneding on the input argument.

## NEDOKONCENE
(delete after final grading)
Nestihol som implementovať prepínač `-2`, prepínače `-i` a `-1` by mali fungovať správne.

Points: 5/12

## Requirements
* ghc version 8.10.7
* Text.Parsec Haskell library (if not installed)

## Build
To build the project use `make` command, project is compiled using `ghc` compiler with option `-Wall` and creates `flp21-fun` executable file.

## Running the project
After building the project, as per section above, it can be run using :

```shell
$ ./flp21-fun [-i|-1] FILE
```
where 
`FILE` is the name of the input file, located in the `test` folder and has the `.in` file extension. Option `-i` runs the program without any changes made to the grammar, simply loads up a grammar from the input file, parses it using `Text.Parsec` Haskell library and prints out onto `stdout` the internal representation with the same formating as the input had. Option `-1` applies the first step of algorithm 4.3, removing non-terminating non-terminals from the grammar and rules containing said non-terminals on either side, before printing the resulting grammar on `stdout`.

## Cleaning after the project

Multiple `.o` and `.hi` files as well as the `flp21-fun` program are made after the build, to clean them up, simply use
```shell
$ make clean
```

## Description

As stated above program implements algorithm 4.3 from TIN class which removes useless symbols, therefore removing unreachable rules, non-terminating terminals and unreachable terminals. Input grammar is validated before parsing to check if each set only contains the correct symbols i.e. non-terminals are only capital letters, terminals are lowercase etc. Parsing is done through `Text.Parsec` library and was inspired by the TuringParse.hs demo from FLP class ([source](https://wis.fit.vutbr.cz/FIT/st/cfs.php.cs?file=%2Fcourse%2FFLP-IT%2Fpclabs%2FTuring-machine%2FTuringParse.hs&cid=14578 "Source")). Application of each algorithm step is done depending on the chosen input argument. Source codes are split into multiple files:
* `Main.hs` - does what you would expect a main file to do, handles input, output and running the whole program
* `ParseInput.hs` - contains parsers for each set in the grammar, master parser for the whole grammar and handles parsing and validity checking before parsing
* `Types.hs` - contains custom data types used throughout the program, most important ones are `Grammar` and `Rule`, the rest could be replaced by standard data types, however naming them after what they represent makes the code visually clearer
* `SimplifyAlg.hs` - handles the whole simplification algorithm (or well just the first half I managed to do)

## Testing

All testing was done purely by running the project with `testX.in` input file and visually comparing the ouput with the correct output saved in `testX.out`, where X is a number of the test. Additionally you can use either `make testi` to load and display input grammar or `make test1` to test the first step of algorithm with `test/test1.in` input.
For example 
```shell
$ ./flp21-fun -1 test/test1.in
```
should return
```
A,C
a
A
A->a
C->a

```
