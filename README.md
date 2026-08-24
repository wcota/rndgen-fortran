# rndgen-fortran

A modern Fortran random-number library centered on **xoshiro256\*\*** as the default generator.

## Overview

Version 2 uses **xoshiro256\*\*** (XOR/shift/rotate, 256-bit state) as the standard engine for `rndgen_t`.

- Original C reference implementation (public domain / CC0):
  <https://prng.di.unimi.it/xoshiro256starstar.c>
- This Fortran implementation is adapted from:
  <https://github.com/fortran-lang/stdlib>
- Additional background and benchmark material:
  <https://prng.di.unimi.it/>

### Why xoshiro256**

- 256-bit internal state
- Long period: $2^{256}-1$
- Excellent speed and statistical quality for general scientific use
- Widespread adoption in modern runtimes and toolchains

## Installation (fpm)

Add this dependency to your `fpm.toml`:

```toml
[dependencies]
rndgen-fortran = { git = "https://github.com/wcota/rndgen-fortran", tag = "v2" }
```

## Quick Start

```fortran
program demo_rndgen
    use iso_fortran_env, only : i4 => int32, i8 => int64, sp => real32, dp => real64
    use rndgen_mod, only : rndgen_t
    implicit none

    type(rndgen_t) :: rng
    call rng%init(42)  ! default integer (typically i4); i8 also works if needed

    print *, rng%rnd()                    ! real(dp) in [0, 1)
    print *, rng%int(1, 10)               ! default integer range; use i4/i8 explicitly if needed
    print *, rng%real(-1.0_dp, 1.0_dp)    ! real(dp) in [-1, 1)
end program demo_rndgen
```

Kind aliases used above:

- `i4 => int32`
- `i8 => int64`
- `sp => real32`
- `dp => real64`

## Main Types

- `rndgen_t`
  Default generator type in v2 (xoshiro256\*\*).
- `rndgen_xoshiro256_t`
  Explicit xoshiro256\*\* type if you prefer to name the engine directly.
- `rndgen_base_t`
  Abstract base class used for polymorphic RNG workflows.
- `rndgen_state_t`
  State container for save/restore operations.

## Main Routines

### Scalar generation

- `rng%rnd()`
  Returns `real(dp)` in [0, 1).
- `rng%rnd_sp()`
  Returns `real(sp)` in [0, 1).
- `rng%int(i1, i2)`
  Returns `integer(i4)` or `integer(i8)` in [i1, i2], based on argument kinds.
- `rng%real(r1, r2)`
  Returns `real(sp)` or `real(dp)` in [r1, r2), based on argument kinds.
- `rng%bool()`
  Returns logical `.true.` or `.false.`.

### Array generation

- `rng%rnd_array(n)`
  Allocates and returns a `real(dp)` array in [0, 1).
- `rng%rnd_array(n, i1, i2)`
  Allocates and returns an `integer(i4)` or `integer(i8)` array in [i1, i2].
- `rng%rnd_array(n, r1, r2)`
  Allocates and returns a `real(sp)` or `real(dp)` array in [r1, r2).
- `rng%bool_array(n)`
  Allocates and returns a logical array.

### In-place filling

- `rng%fill_array(arr)` for reals
- `rng%fill_array(arr, i1, i2)` for integers
- `rng%fill_array(arr, r1, r2)` for ranged reals
- `rng%fill_array(arr)` for logical arrays

### State and reproducibility

- `call rng%init(seed)` initializes with deterministic seed
- `call rng%reset()` resets to original seed
- `state = rng%get_state()` snapshots current engine state
- `call rng%set_state(state)` restores engine state
- `call state%save_state(unit_or_filename)` persists state
- `call state%read_state(unit_or_filename)` reads persisted state

Example:

```fortran
program demo_state
    use iso_fortran_env, only : i8 => int64
    use rndgen_mod, only : rndgen_t, rndgen_state_t
    implicit none

    type(rndgen_t) :: rng
    type(rndgen_state_t) :: st

    call rng%init(2026_i8)
    print *, rng%rnd()

    st = rng%get_state()
    call st%save_state("rng.state")

    print *, rng%rnd()
    print *, rng%rnd()

    call st%read_state("rng.state")
    call rng%set_state(st)

    print *, rng%rnd()  ! repeats from saved position
end program demo_state
```

## KISS Compatibility

⚠️ `rndgen_kiss_t` is still available in `rndgen_kiss_mod` for compatibility with legacy workflows.

KISS is **not** the default in v2 and is planned to be removed from the main package (or split into a compatibility package) in a future major release.

```fortran
program demo_kiss
    use rndgen_kiss_mod, only : rndgen_kiss_t
    implicit none

    type(rndgen_kiss_t) :: rng

    call rng%init(12345)
    print *, rng%rnd()
end program demo_kiss
```

## Examples and Tests

This repository focuses on the library source. For comprehensive usage examples, benchmarks, and tests, see:

- <https://github.com/wcota/rndgen-fortran-examples>

If you cloned that repository:

```bash
fpm run --example simple
fpm test
```

## Breaking Changes (v1 -> v2)

The most important migration points are summarized below.

- Default engine changed from KISS to xoshiro256\*\*
- Main user type renamed from `rndgen` to `rndgen_t`
- Seed/state API redesigned (`rndSeed`, `save_seed`, `read_seed` were replaced)

A full migration guide with before/after code is available in:

- [MIGRATING.md](MIGRATING.md)

## License Notes

- This project is distributed under the repository license.
- The xoshiro256\*\* reference algorithm is in the public domain (CC0), per the original authors and site.
