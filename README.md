# Fortran module: RNDGEN

- Simple implementation, ready for multiple random independent generators

### How to compile and run:

```
$ gfortran examples/example.f90 -o example
$ ./example
```

Expected output: `examples/output-example.txt`

**Using two independent generators:**

```
$ gfortran examples/example-2gen.f90 -o example-2gen
$ ./example-2gen 21382144 21382147 # seed1 seed2
```

Expected output: `examples/output-example-2gen.txt`


### Commands:

- Include the file in your code: ``
- Use the module after the `program`: `use mod_rndgen`
- Declare a generator as `type(rndgen) :: generator`
- Initialize it with a seed: `call generator%init(23242394)`
- Print a random real number U(0,1): `print*, generator%rnd()`
- Print a integer between `a` and `b`: `print*, generator%int(a,b)`
- Reset the generator: `call generator%reset()`

#### TODO: instructions of rndgenPL

