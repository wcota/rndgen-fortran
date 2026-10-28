---
title: rndgen_stats_mod
---

# rndgen_stats_mod

**Source**: `src/rndgen_stats_mod.f90`

**Dependencies**

```mermaid
graph LR
  rndgen_stats_mod["rndgen_stats_mod"] --> iso_fortran_env["iso_fortran_env"]
  rndgen_stats_mod["rndgen_stats_mod"] --> rndgen_mod["rndgen_mod"]
```

## Contents

- [rndgen_stats_t](#rndgen-stats-t)

## Derived Types

### rndgen_stats_t

**Inheritance**

```mermaid
classDiagram
  rndgen_stats_t <|-- rndgen_pl_t
```

#### Components

| Name | Type | Attributes | Description |
|------|------|------------|-------------|
| `rng` | class([rndgen_base_t](/api/rndgen_mod#rndgen-base-t)) | allocatable |  |

#### Type-Bound Procedures

| Name | Attributes | Description |
|------|------------|-------------|
| `init` |  |  |
