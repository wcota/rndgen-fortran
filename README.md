# Fortran module: RNDGEN

- Simple implementation, ready for multiple random independent generators

### How to compile and run:

```
$ ifort mod_rndgen_multiple.f90 example.f90
$ ./a.out
```

Expected output: `output-example.txt`

**Using two independent generators:**

```
$ ifort mod_rndgen_multiple.f90 example-2gen.f90
$ ./a.out 21382144 21382147 # seed1 seed2
```

Expected output: `output-example-2gen.txt`


### Commands:

- Declare a generator as `type(rndgen) :: gerador`
- Initialize it with a seed: `call gerador%init(23242394)`
- Print a random real number U(0,1): `print*, gerador%rnd()`
- Print a integer between `a` and `b`: `print*, gerador%int(a,b)`
- Reset the generator: `call gerador%reset()`

#### TODO: instructions of rndgenPL
