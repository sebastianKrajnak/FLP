# Rubiks cube solver

Rubiks cube solver coded in Prolog. Receives a state of a rubiks cube defined as
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

## PROBLEMY, BUGY, FUNKCNOST
Solver vyrieši obidve testované kocky úspešne avšak vykonáva zbytočné rotácie až pokiaľ nebude counter na maxime tj 7

## Requirements
* ghc version 8.4.2

## Build
To build the project use `make` command, project is compiled using `swipl` compiler and creates `flp21-fun` executable file.

## Running the project
After building the project, as per section above, it can be run using :
```shell
$ make run
```
which will automatically run both tests and meassure the runtime or use 
```shell
$ ./flp21-log < INPUT_FILE
```
to manually run the solver, where `INPUT_FILE` is the name of the input file, located in the `test` folder and has the `.in` file extension.

## Cleaning after the project

`flp21-log` program is made after the build, to clean it up, simply use
```shell
$ make clean
```

## Description
Loading and parsing the data from the input file is purely copied from a provided example from class located at ([input2.pl](https://wis.fit.vutbr.cz/FIT/st/cfs.php.cs?file=%2Fcourse%2FFLP-IT%2Fprojects%2Flog%2Finput2.pl&cid=14578 "Source")). All possible rotations (UP, FRONT, LEFT, RIGHT, BACK, DOWN) of the cube 6 for clockwise and 6 for counter clockwise rotation have been hardcoded using unification. Predicate `check_solve` simply checks if all sides have only one colour on each side. `map_cube` predicate is uded to map the cube from the parsed format into the format used in the implementation. The solver uses DLS with the max depth of 8 to recursively search through all possible rotations of the cube until it finds a orrect one.

## Testing

All testing was done purely by running the project with `test1.in`, `test2.in` input files and visually comparing the ouput with the correct output which is
```
555
555
555
111 222 333 444
111 222 333 444
111 222 333 444
666
666
666
```
