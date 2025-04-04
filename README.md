# Fortran Module: RNDGEN

## Description

The Fortran module `RNDGEN` implements the KISS (Keep It Simple Stupid) random number generator as an object, facilitating the usage of multiple and independent generators.

The code adaptation is derived from [Thomas Vojta](http://thomasvojta.com/)'s implementation available at [http://web.mst.edu/~vojtat/class_5403/kiss05/rkiss05.f90](http://web.mst.edu/~vojtat/class_5403/kiss05/rkiss05.f90).

For reference, the original source code can be found [here](http://web.mst.edu/~vojtat/class_5403/kiss05/rkiss05.f90), with the following information:

```txt
! Random number generator KISS05 after a suggestion by George Marsaglia
! in "Random numbers for C: The END?" posted on sci.crypt.random-numbers
! in 1999
!
! version as in "double precision RNGs" in  sci.math.num-analysis  
! http://sci.tech-archive.net/Archive/sci.math.num-analysis/2005-11/msg00352.html
!
! The  KISS (Keep It Simple Stupid) random number generator. Combines:
! (1) The congruential generator x(n)=69069*x(n-1)+1327217885, period 2^32.
! (2) A 3-shift shift-register generator, period 2^32-1,
! (3) Two 16-bit multiply-with-carry generators, period 597273182964842497>2^59
! Overall period > 2^123  
! 
! 
! A call to rkiss05() gives one random real in the interval [0,1),
! i.e., 0 <= rkiss05 < 1
!
! Before using rkiss05 call kissinit(seed) to initialize
! the generator by random integers produced by Park/Millers
! minimal standard LCG.
! Seed should be any positive integer.
! 
! FORTRAN implementation by Thomas Vojta, vojta@mst.edu
! built on a module found at www.fortran.com
! 
! 
! History:
!        v0.9     Dec 11, 2010    first implementation
!        V0.91    Dec 11, 2010    inlined internal function for the SR component
!        v0.92    Dec 13, 2010    extra shuffle of seed in kissinit 
!        v093     Aug 13, 2012    changed inter representation test to avoid data statements
```

## Usage

Copy the file `src/rndgen.f90` or add the package as a dependency using the [Fortran Package Manager](https://fpm.fortran-lang.org/) (Fpm):

```toml
[dependencies]
rndgen-fortran.git = "https://github.com/wcota/rndgen-fortran"
```

Import the module using `use rndgen_mod`, and/or `use rndgenPL_mod` for the power-law number generator.

Create the generator object by using

```fortran
type(rndgen) :: generator
```

The generator requires a positive integer as a seed to initialize the generator with Park/Millers minimal standard LCG:

```fortran
integer :: seed = 294727492
call generator%init(seed)
```

To generate a random number, use:

- `generator%rnd()` for a real number in the range [0,1)
- `generator%int(i1, i2)` for an integer number in the range [i1, i2]
- `generator%real(r1, r2)` for a real number in the range [r1, r2)
- `generator%rnd_array(n)` for a real array with numbers in range [0,1) with size `n`
- `generator%rnd_array(n,i1,i2)` for a integer array with numbers in the range [i1, i2] with size `n`
- `generator%rnd_array(n,r1,r2)` for a real array with numbers in the range [r1, r2) with size `n`

Reset the generator (to start again with the same sequence) with `generator%reset()`.

### `rndSeed` IO

It is possible to save and read the generated seeds. For that, an `rndSeed` object needs to be declared as

```fortran
type(rndSeed) :: seeds
```

To save, use `call generator%save_seed(seeds, file_unit)`, where `file_unit` refers to an opened file unit. It will save the current seed in the object and also write it to the file unit.

To read from a `rndSeed` object, use `call generator%read_seed(seeds)`, or to read from a file unit, use `call generator%read_seed(seeds, file_unit)`.

### Power-law random number generator

The module `rndgenPL_mod` extends the generator to an integer power-law distribution. The code was adapted from [Silvio C. Ferreira](https://sites.google.com/site/silvioferreirajr/home)'s codes.

Declare the object with `type(rndgenPL) :: generatorPL`, and initialize it with `call generatorPL%initPL(kmin, kmax, gamma, seed)`, allowing the generation of random numbers following a power-law $P(k) \sim k^{-\gamma}$ for $k \in [k_\text{min}, k_\text{max}]$.

To generate the number, use `generatorPL%rndPL()`.

To generate an array of size `n`, use `generatorPL%rndPL_array(n)`

## Running examples

Using Fpm, execute:

```bash
fpm run --example example
```

to run the first example. The list of examples is as follows:

- `simple`: Generates 10 random numbers between 0 and 1, integers between 5 and 2587, and floats between -5.2 and 100.9, resets, and repeats the process.
- `arrays`: Generates 10 random numbers between 0 and 1, integers between 5 and 2587, and floats between -5.2 and 100.9, resets, and repeats the process, using the array generator
- `vojta`: Original example by Thomas Vojta, from <https://web.mst.edu/vojtat/class_5403/kiss05/rtest.f90>
- `save`: Generates 10 random numbers, resets, and saves the state after 5 runs. Then, reads from the file.
- `2gen`: Runs two generators simultaneously. Usage: `fpm run --example example-2gen -- seed1 seed2`
- `2gen-invert`: Same as the previous example, but swaps the seeds after a reset.
- `PL`: Generates four sequences of power-law distributed random numbers.

Expected outputs are available at `example/output-*.txt`.

Tested with `gfortran`, `ifort`, and `ifx` compilers. To use a specific compiler, run with `fpm --compiler=ifort [...]`.