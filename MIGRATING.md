# Migrating from v1 to v2

This guide highlights the **breaking changes** introduced in `rndgen-fortran` v2 and how to update existing code.

## Quick API Map (Old -> New)

| v1 (old) | v2 (new) |
|---|---|
| `type(rndgen)` | `type(rndgen_t)` |
| `type(rndSeed)` | `type(rndgen_state_t)` |
| `call rng%save_seed(seeds, unit_or_file)` | `state = rng%get_state()` + `call state%save_state(unit_or_file)` |
| `call rng%read_seed(seeds, unit_or_file)` | `call state%read_state(unit_or_file)` + `call rng%set_state(state)` |
| default engine: KISS | default engine: xoshiro256** (`rndgen_t`) |
| KISS via main type | KISS via `rndgen_kiss_t` in `rndgen_kiss_mod` |

## Summary of Breaking Changes

1. Default RNG changed from **KISS** to **xoshiro256\*\***.
2. Main generator type renamed from `rndgen` to `rndgen_t`.
3. Seed/state API was redesigned:
   - `rndSeed` -> `rndgen_state_t`
   - `save_seed` / `read_seed` -> `get_state` / `set_state` + `save_state` / `read_state`
4. KISS moved to compatibility module usage (`rndgen_kiss_mod`, `rndgen_kiss_t`).

## 1) Default Generator Changed

### v1 behavior

- `rndgen` used KISS as the implicit default.

### v2 behavior

- `rndgen_t` uses xoshiro256\*\* by default.

If you need old KISS-compatible streams, instantiate `rndgen_kiss_t` explicitly.

## 2) Type Rename: rndgen -> rndgen_t

### Before (v1)

```fortran
use rndgen_mod
implicit none

type(rndgen) :: rng
call rng%init(12345)
print *, rng%rnd()
```

### After (v2)

```fortran
use rndgen_mod, only : rndgen_t
implicit none

type(rndgen_t) :: rng
call rng%init(12345)
print *, rng%rnd()
```

## 3) State API Redesign (Important)

This is the largest user-facing change.

### Before (v1)

```fortran
use rndgen_mod
implicit none

type(rndgen) :: rng
type(rndSeed) :: seeds
integer :: unit

call rng%init(2026)
open(newunit=unit, file="seed.dat", status="replace", action="write")
call rng%save_seed(seeds, unit)
close(unit)

! ... later ...
open(newunit=unit, file="seed.dat", status="old", action="read")
call rng%read_seed(seeds, unit)
close(unit)
```

### After (v2)

```fortran
use rndgen_mod, only : rndgen_t, rndgen_state_t
implicit none

type(rndgen_t) :: rng
type(rndgen_state_t) :: state

call rng%init(2026)
state = rng%get_state()
call state%save_state("seed.dat")

! ... later ...
call state%read_state("seed.dat")
call rng%set_state(state)
```

Notes:

- In v2, the state object is independent from the RNG object.
- You can persist state either by filename or by already-opened unit.
- Save/restore now captures the full engine state explicitly.

## 4) KISS Compatibility in v2

KISS is still available for migration scenarios and reproducibility checks.

```fortran
use rndgen_kiss_mod, only : rndgen_kiss_t
implicit none

type(rndgen_kiss_t) :: rng
call rng%init(12345)
print *, rng%rnd()
```

Important:

- KISS is no longer the main path.
- Plan future migrations to `rndgen_t` (xoshiro256\*\*).

## 5) Practical Migration Checklist

1. Replace `type(rndgen)` with `type(rndgen_t)`.
2. Update module imports to include explicit `only` lists where useful.
3. Replace `rndSeed` with `rndgen_state_t`.
4. Replace `save_seed` and `read_seed` calls with:
   - `state = rng%get_state()`
   - `call rng%set_state(state)`
   - `call state%save_state(...)`
   - `call state%read_state(...)`
5. If you require KISS compatibility, move those call sites to `rndgen_kiss_t`.

## Related Resources

- xoshiro family reference: <https://prng.di.unimi.it/>
- xoshiro256** C reference (public domain / CC0):
  <https://prng.di.unimi.it/xoshiro256starstar.c>
- Examples and tests repository:
  <https://github.com/wcota/rndgen-fortran-examples>
