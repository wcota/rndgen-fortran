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
    use rndgen_mod, only : rndgen_t
    implicit none

    type(rndgen_t) :: rng

    call rng%init(42)

    print *, rng%rnd()            ! real(8) in [0, 1)
    print *, rng%int(1, 10)       ! integer in [1, 10]
    print *, rng%real(-1.0d0, 1.0d0) ! real(8) in [-1, 1)
    print *, rng%bool()           ! logical
end program demo_rndgen
```

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
  Returns `real(8)` in [0, 1).
- `rng%int(i1, i2)`
  Returns integer in [i1, i2].
- `rng%real(r1, r2)`
  Returns real in [r1, r2).
- `rng%bool()`
  Returns logical `.true.` or `.false.`.

### Array generation

- `rng%rnd_array(n)`
  Allocates and returns an array of random reals in [0, 1).
- `rng%rnd_array(n, i1, i2)`
  Allocates and returns an integer array in [i1, i2].
- `rng%rnd_array(n, r1, r2)`
  Allocates and returns a real array in [r1, r2).
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
    use rndgen_mod, only : rndgen_t, rndgen_state_t
    implicit none

    type(rndgen_t) :: rng
    type(rndgen_state_t) :: st

    call rng%init(2026)
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
