# Fortran module: RNDGEN

## multiple 

- way to use multiple random independent generators

### How to compile and run:

```
$ ifort mod_rndgen_multiple.f90 exemplo.f90
$ ./a.out
```

Expected output: `saida-exemplo.txt`

**Using two independent generators:**

```
$ ifort mod_rndgen_multiple.f90 exemplo-2geradores.f90
$ ./a.out 21382144 21382147
```

Expected output: `saida-exemplo-2geradores.txt`


### Commands:

- Declare a generator as `type(rndgen) :: gerador`
- Initialize it with a positive seed: `call gerador%init(23242394)`
- Print a random real number U(0,1): `print*, gerador%rnd()`
- Print a integer between `a` and `b`: `print*, gerador%int(a,b)`
- Reset the generator: `call gerador%reset()`


## single

- just a clean implementation of KISS(), but just a single generator

### How to compile and run:

```
$ ifort mod_rndgen_single.f90 exemplo.f90
$ ./a.out
```

Expected output: `saida.txt`. NOTE: is the same output of multiple/exemplo.f91.

### Commands:

- Initialize it with a positive seed: `call rnd_init(23242394)`
- Print a random real number U(0,1): `print*, rnd()`
- Print a integer between `a` and `b`: `print*, rnd_int(a,b)`
- Reset the generator: `call rnd_reset()`