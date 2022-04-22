# Rubiks cube solver

Rubics cube solver coded in Prolog. Receives a state of a rubiks cube defined as
```
UUU
UUU
UUU
FFF RRR BBB LLL
FFF RRR BBB LLL
FFF RRR BBB LLL
DDD
DDD
DDD
```
where `U` is the top of the cube, `F` is the front, `R` and `L` are the left and right, `B` is the back and `D` is the bottom of the cube. The output is a sequence of moves that lead to a solved cube.

## Requirements
* ghc version 8.4.2

## Build
To build the project use `make` command, project is compiled using `swipl` compiler and creates `flp21-fun` executable file.

## Running the project
After building the project, as per section above, it can be run using :

```shell
$ ./flp21-log < INPUT_FILE
```
where 
`INPUT_FILE` is the name of the input file, located in the `test` folder and has the `.in` file extension.

## Cleaning after the project

`flp21-log` program is made after the build, to clean it up, simply use
```shell
$ make clean
```

## Description

## Testing

All testing was done purely by running the project with `testX.in` input file and visually comparing the ouput with the correct output saved in `testX.out`, where X is a number of the test. Additionally you can use either `make testi` to load and display input grammar or `make test1` to test the first step of algorithm with `test/test1.in` input.
For example 
```shell
$ ./flp21-log < test/test1.in
```
should return
```
A,C
a
A
A->a
C->a

```