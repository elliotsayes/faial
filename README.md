# GPU Contracts

A modeling language to specify and debug GPU programs.

## Installation

### 1. Setup

**Run this once.** The following command will install all dependencies needed to build the project.

```bash
./configure.sh
```

### 2. Build

**Run this to build the binary.**

```bash
$ make
```

### [Download `main` for Linux x86-64bits](https://gitlab.com/cogumbreiro/faial/-/jobs/artifacts/master/download?job=build-dist)


## External dependencies

* [z3](https://github.com/Z3Prover/z3)
* [opam](https://opam.ocaml.org/)
* [bash](https://www.gnu.org/software/bash/)


# Usage

```
./main examples/race1.proto2 | z3 -in
```

For more information run `./main --help`

# Tutorial

## The SPMD programming model

The programming model of a GPU contract is an
[SPMD](https://en.wikipedia.org/wiki/SPMD) execution. The programming model
assumes the concurrent execution by multiple tasks, each of which with a local
memory and a global memory. All tasks execute the same program concurrently,
their state only differing in their local memory.

The only way tasks can synchronize is by issuing a `sync`, an all-to-all
synchronization, a task issuing a `sync` blocks until all other tasks also issue
a `sync`.


## A GPU contract

A GPU contract models a GPU program, programmed in an SPMD programming models. A
GPU contract only represents **shared memory accesses** and **synchronization**,
along with minimal control flow that coordinates the described accesses and
synchronization.

## Example 1: the contract preamble and a memory access

**Summary.** Example [`race1`](examples/race1.proto2): introduces comments, shared locations, global constants, and a memory access.

A GPU contract starts with a preamble where we can declare the set of all
shared locations and also the set of all variables used in the contract.

*Shared locations.* Every shared location corresponds to an array of an
unspecified length. Shared locations are accessible by all tasks, they are all **global**. Shared locations are not scoped lexically, which means that every shared location
is visible to the whole contract.

We can declare a shared location `x` with the following definition:
```
shared x;
```

The location declaration section is comma-separated and terminated with a semi-colon. We can also declare three shared locations as follows.
```
shared x, y, z;
```

*Global constants.* Global constants can only be declared in the preamble.
All constants declared are unsigned integers of an arbitrary size.
These are constants that have the same value throughout the whole computation and have the same value to all tasks. Global constants typically
represent constants declared in C with `#define`, thus available at compile time.

The following line declares one constant `ARRAY_SIZE`.

```
const global ARRAY_SIZE;
```

The `const` section allows for multiple constant declaration, comma-separated and semi-colon terminated. The following line declares two global constants `ARRAY_SIZE` and `COUNT`.

```
const global ARRAY_SIZE, global COUNT;
```

*Memory accesses.* Memory accesses can only be written after the preamble. Each is prefixed by the access mode either `rw` for read-write access, or `ro` for read-only access. Next, we specify the location, an expression that specifies the index, and optionally a condition enables or disables the access.

For instance, the following line declares that all tasks will concurrently
write to array `x` at index 0.
```
rw x[0];
```

Next, the following specifies that all tasks will read from a position
`SIZE`.
```
ro x[SIZE];
```

Finally, we show another example where the access is conditional:

```
rw x[SIZE] if SIZE % 2 == 0;
```
Where we specify that all tasks may write to position `SIZE` of the array,
but only if `SIZE` is an even number.

## Example 2: modeling iteration and synchronization

**Summary.** Example [`race2`](examples/race2.proto2) introduces `foreach` and
synchronization.

*Modeling iteration, looping with `foreach`.* A `foreach` describes how a
variable changes in a looping construct. It defines a variable that ranges from
0 until an upper bound. Inside the loop we may further constrain the
range of possible values bound to the loop variable.

For instance, in the code below we specify that a variable `x` ranges from 0 up
to 9 (inclusively) and that all tasks read position `x` from array `buffer`.

```
foreach x < 10 {
  ro buffer[x];
}
```

We can further restrict the possible values being read to only the even
elements the array `buffer` with the following code, which will read
elements 0, 2, 4, 6, and 8.

```
foreach x < 10 {
  ro buffer[x] if x % 2 == 0;
}
```

Alternatively, we can use an `assert` to restrict variable `x` in the lexical scope defined inside the braces.

```
foreach x < 10 {
  assert x % 2 == 0;
  ro buffer[x];
}
```

If we wish to read elements 4, 6, and 8 we may write the following code:

```
foreach x < 10 {
  assert x >= 4;
  assert x % 2 == 0;
  ro buffer[x];
}
```

*Synchronization.* Tasks can synchronize with `sync`. For instance, we can
ensure all tasks read position 0 from `buffer`, then all tasks must wait for
each other, and finally all tasks read position 1 from the array.
```
ro buffer[0];
sync;
ro buffer[1];
```


## Example 3: thread-local variables and data-race freedom

**Summary.** Example [`ok`](examples/ok.proto2) introduces thread-local constants. First data-race free example.

## More examples

* Example 4 [`customIntegrator`](examples/customIntegrator.proto2): a model of a real kernel. Should be compared against the original code.
* Example 5 [`customIntegrator-applyPositionDeltas1`](examples/customIntegrator-applyPositionDeltas1.proto2): a more complicated model of a real CUDA kernel. It includes standard CUDA restrictions.



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
