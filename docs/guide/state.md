# State & Reproducibility

## Seeding and Reset

```fortran
call rng%init(seed)   ! deterministic: same seed, same stream (i4 or i8 seed)
call rng%reset()      ! restart the stream from the original seed
```

For xoshiro256\*\*, the seed is expanded into the 256-bit state with SplitMix64,
so any 64-bit value (including zero) is a valid seed.

## Snapshots

`rndgen_state_t` holds the full engine state, independently of the generator object:

| Call | Effect |
|---|---|
| `state = rng%get_state()` | snapshot of the current engine state |
| `call rng%set_state(state)` | restore the engine state |
| `call state%save_state(unit_or_filename)` | write the state to an open unit or a file |
| `call state%read_state(unit_or_filename)` | read the state from an open unit or a file |

## Example

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

This is the typical pattern for checkpoint/restart of long simulations: save the
state together with the simulation data, and restore it when resuming.
