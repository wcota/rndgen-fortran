# Generating Numbers

All routines below are available on every generator extending `rndgen_base_t`
(`rndgen_t`, `rndgen_xoshiro256_t`, `rndgen_kiss_t`). The integer and real
variants are resolved by the kinds of the arguments.

## Scalars

| Call | Result |
|---|---|
| `rng%rnd()` | `real(dp)` in $[0, 1)$ |
| `rng%rnd_sp()` | `real(sp)` in $[0, 1)$ |
| `rng%int(i1, i2)` | `integer(i4)` or `integer(i8)` in $[i_1, i_2]$ |
| `rng%real(r1, r2)` | `real(sp)` or `real(dp)` in $[r_1, r_2)$ |
| `rng%bool()` | `.true.` or `.false.` with equal probability |

```fortran
use iso_fortran_env, only : i8 => int64, dp => real64
use rndgen_mod, only : rndgen_t

type(rndgen_t) :: rng
call rng%init(42)

print *, rng%rnd()                       ! real(dp) in [0, 1)
print *, rng%int(1, 6)                   ! integer(i4) in [1, 6]
print *, rng%int(1_i8, 10000000000_i8)   ! integer(i8)
print *, rng%real(-1.0_dp, 1.0_dp)       ! real(dp) in [-1, 1)
print *, rng%bool()
```

## Allocating Arrays

| Call | Result |
|---|---|
| `rng%rnd_array(n)` | `real(dp)` array in $[0, 1)$ |
| `rng%rnd_array(n, i1, i2)` | `integer(i4)` or `integer(i8)` array in $[i_1, i_2]$ |
| `rng%rnd_array(n, r1, r2)` | `real(sp)` or `real(dp)` array in $[r_1, r_2)$ |
| `rng%bool_array(n)` | logical array |

```fortran
real(dp), allocatable :: u(:)
integer, allocatable :: dice(:)

u    = rng%rnd_array(1000)
dice = rng%rnd_array(1000, 1, 6)
```

## In-place Filling

Use `fill_array` to reuse an existing array and avoid allocations in hot loops:

| Call | Fills with |
|---|---|
| `call rng%fill_array(arr)` | `real(dp)` in $[0, 1)$, or logical values for a logical `arr` |
| `call rng%fill_array(arr, i1, i2)` | integers in $[i_1, i_2]$ |
| `call rng%fill_array(arr, r1, r2)` | reals in $[r_1, r_2)$ |

```fortran
real(dp) :: buffer(4096)

do step = 1, nsteps
    call rng%fill_array(buffer)
    ! ... use buffer ...
end do
```
