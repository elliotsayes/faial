# GPU Contracts

A modeling language to specify and debug GPU programs.

# Setup

The following command will install all dependencies needed to build the project.

```bash
./configure.sh
```

## External dependencies

* [z3](https://github.com/Z3Prover/z3)
* [opam](https://opam.ocaml.org/)
* [bash](https://www.gnu.org/software/bash/)

# Build

```bash
$ make
```

# Usage

```
./main examples/race1.proto2 | z3 -in
```

For more information run `./main --help`

# Tutorial

1. [`race1`](examples/race1.proto2): Introduces comments, shared locations, global constants, and a memory access
2. [`race2`](examples/race2.proto2): Introduces `foreach`, synchronization
3. [`ok`](examples/ok.proto2): Introduces thread-local constants. First data-race free example.
4. [`customIntegrator`](examples/customIntegrator.proto2): a model of a real kernel. Should be compared against the original code.
5. [`customIntegrator-applyPositionDeltas1`](examples/customIntegrator-applyPositionDeltas1.proto2): a more complicated model of a real CUDA kernel. It includes standard CUDA restrictions.

# Translating loop strides

## Multiples of `k`

```C
for (index = INIT; index < UB; index += STRIDE) {
  ...
}
```

is translated as

```perl
foreach index < UB {
  assert index >= INIT;
  assert (index - INIT) % STRIDE == 0;
  ...
}
```

## Powers of two

```C
for (index = INIT; index < UB; i *= 2) {
  ...
}
```
is translated as
```perl
foreach index < UB {
  assert index >= INIT;
  assert pow2(index);
  ...
}
```

The translation is the same for:

```C
for (index = INIT; index < UB; i >>= 1) {
  ...
}
```
