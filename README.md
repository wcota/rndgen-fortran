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

## single

- just a clean implementation of KISS(), but just a single generator

### How to compile and run:

```
$ ifort mod_rndgen_single.f90 exemplo.f90
$ ./a.out
```

Expected output: `saida.txt`. NOTE: is the same output of multiple/exemplo.f90.