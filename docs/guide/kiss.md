# KISS Compatibility

::: warning
`rndgen_kiss_t` is kept in `rndgen_kiss_mod` for compatibility with legacy workflows.
KISS is **not** the default in v2 and is planned to be removed from the main package
(or split into a compatibility package) in a future major release.
:::

KISS05 (Marsaglia, as implemented by Thomas Vojta) combines a congruential generator,
a 3-shift shift-register generator and two multiply-with-carry generators, with an
overall period above $2^{123}$. It was the default engine of `rndgen-fortran` v1.

Use it only when you need to reproduce streams generated with v1:

```fortran
program demo_kiss
    use rndgen_kiss_mod, only : rndgen_kiss_t
    implicit none

    type(rndgen_kiss_t) :: rng

    call rng%init(12345)
    print *, rng%rnd()
end program demo_kiss
```

`rndgen_kiss_t` extends `rndgen_base_t`, so all routines from
[Generating Numbers](./usage.md) and the state API from
[State & Reproducibility](./state.md) work the same way. `jump` and `long_jump`
are only available for xoshiro256\*\*.
