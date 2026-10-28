---
layout: home

hero:
  name: "rndgen-fortran"
  text: "Random Numbers for Modern Fortran"
  tagline: "xoshiro256** by default: fast, 256-bit state, reproducible and ready for parallel runs."
  actions:
    - theme: brand
      text: Get Started
      link: /guide/
    - theme: alt
      text: API Reference
      link: /api/
    - theme: alt
      text: GitHub Repository
      link: https://github.com/wcota/rndgen-fortran

features:
  - title: xoshiro256** by default
    details: 256-bit state, period 2^256 - 1, excellent speed and statistical quality. Output matches the C reference implementation.
  - title: Scalars, arrays and ranges
    details: Uniform reals in [0, 1), integers in [i1, i2], ranged reals and booleans, for both 32- and 64-bit kinds.
  - title: Reproducible state
    details: Snapshot, restore, save and read the full generator state, by file name or by unit.
  - title: Parallel streams
    details: jump() and long_jump() split one seed into non-overlapping subsequences for threads and processes.
---
