# Tutorial

# Checking a CUDA kernel

We check a [SAXPY CUDA example](https://developer.nvidia.com/blog/easy-introduction-cuda-c-and-c/).

```c
$ cat saxpy.cu
__global__
void saxpy(int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (i < n) y[i] = a*x[i] + y[i];
}
```

Running `faial` without command line options checks the protocol for DRF.

```
$ faial saxpy.cu
Program is data-race free!
```


## Checking a racy protocol

Let us now check a buggy protocol `saxpy-buggy.cu`. The difference between `saxpy.proto` and `saxpy-buggy.cu` is simply changing the last read from `y[i]` into `y[i + 1]`.


```
$ diff -u saxpy.cu saxpy-buggy.cu 
--- saxpy.cu	2021-04-19 16:28:24.407379028 -0400
+++ saxpy-buggy.cu	2021-04-20 10:41:26.317324409 -0400
@@ -2,5 +2,5 @@
 void saxpy(int n, float a, float *x, float *y)
 {
   int i = blockIdx.x*blockDim.x + threadIdx.x;
-  if (i < n) y[i] = a*x[i] + y[i];
+  if (i < n) y[i] = a*x[i] + y[i + 1];
 }
\ No newline at end of file
```


We can now check the buggy kernel.

```
$  faial saxpy-buggy.cu 
*** DATA RACE ERROR ***

Array:   y[1]
T1 mode: W
T2 mode: R

-------------------
 Globals     Value 
-------------------
 blockDim.x  2 
-------------------
 blockIdx.x  0 
-------------------
 gridDim.x   1 
-------------------
 n           2 
-------------------

---------------------
 Locals       T1  T2 
---------------------
 threadIdx.x  1   0 
---------------------
```

The error report consists of:
 * the array name and index, here the data-race occurs on `y[1]`
 * the access mode of the two logical threads involved in the data-race (labeled `T1` and `T2`). In this example, we have a read (`T1 mode: R`) and a write (`T2 mode: W`)
 * the runtime state of thread-global variables (table `Globals`). We can
   observe that in this example having two threads (`blockDim.x=2`) is
   sufficient to trigger the data-race.
 * the runtime state of thread-local variables (table `Locals`). There is one
   column per thread causing the data-race. In this case one thread
   `threadIdx.x=0` races with tread `threadIdx.x=1`.


# The protocol language

In addition to CUDA kernels, Faial allows for the verification
of a protocol stored in the `proto` file type as shown.

We show the equivalent `proto` of our SAXPY example.


```
$ cat saxpy.proto
shared x, y;

const
  local threadIdx.x,
  global blockIdx.x,
  global blockDim.x,
  global gridDim.x,
  global n,
  where distinct [threadIdx.x]
    && threadIdx.x < blockDim.x
    && blockIdx.x < gridDim.x;

let i = blockIdx.x*blockDim.x + threadIdx.x;
if (i < n) {
  rw y[i];
  ro x[i];
  ro y[i];
}
```

A protocol has 3 sections:
* section `shared` (separated by commas, terminated by a semi-colon): we declare the arrays being checked.
* section `const`: we declare the protocol parameters.
    * a `local` parameter declares
  a thread-`local` parameter, ie, each thread may assign a different value to that variable.
    * a `global` parameter declares a thread-`global` parameter, ie, all threads observe the same value.
    * a `where` clause (must be appear last in this section): further restricts the range of values being assigned to the parameters declared. These can be boolean expressions
    or a `distinct` clause that makes an expression.
* the protocol itself.

In this example, we declare two arrays `x` and `y`. We then declare four
CUDA-specific parameters: a thread-local parameter `threadIdx.x` and three
thread-global parameters `blockIdx.x`, `blockDim.x`, `gridDim.x`. We also define
a global parameter `n` as in the original example. In our `where` we establish
the standard CUDA-specific constraints: `threadIdx.x` is unique per-thread,
`blockDim.x` is the upper bound of `threadIdx.x`, and `gridDim.x` is the upper
bound of `blockIdx.x`.

As for the protocol itself, we define a temporary variable `i` and inside the
conditional we perform one write (`rw y[i]`) and two reads (`ro x[i]` and `ro
y[i]`), following the CUDA example. Note that because there is no barrier
synchronization, the order of the accesses is irrelevant.

Checking our protocol is trivial:
```
$ faial saxpy.proto 
Program is data-race free!
```

## Protocol inference

Finally, instead of checking for DRF, we can inspect the internal representation
of `faial` to ensure that the inferred protocol is accurate. The following is an
internal representation of the protocol, so its syntax differs slightly.

```bash
$ faial -A --steps 1 saxpy.cu 
arrays: y, x;
globals: n, gridDim.x, blockIdx.x, blockDim.x;
locals: threadIdx.x, i;
invariant: ((proj($T1, threadIdx.x) != proj($T2, threadIdx.x) && threadIdx.x < blockDim.x) && blockIdx.x < gridDim.x) && (blockIdx.x * blockDim.x) + threadIdx.x < n;

code {
    rw y[(blockIdx.x * blockDim.x) + threadIdx.x];
    ro x[(blockIdx.x * blockDim.x) + threadIdx.x];
    ro y[(blockIdx.x * blockDim.x) + threadIdx.x];
}
```
Of note:
* variable `i` is inlined in the protocol
* the condition `i < n` was hoisted to the `invariants` section (which is akin to the `where` clause in our protocol language)

Similarly, we can emit the internal representation of `saxpy.proto`:

```
$ faial saxpy.proto  -A --steps 1
arrays: y, x;
globals: n, gridDim.x, blockIdx.x, blockDim.x;
locals: threadIdx.x;
invariant: (proj($T1, threadIdx.x) != proj($T2, threadIdx.x) && threadIdx.x < blockDim.x) && blockIdx.x < gridDim.x;

code {
    if ((blockIdx.x * blockDim.x) + threadIdx.x < n) {
        rw y[(blockIdx.x * blockDim.x) + threadIdx.x];
        ro x[(blockIdx.x * blockDim.x) + threadIdx.x];
        ro y[(blockIdx.x * blockDim.x) + threadIdx.x];
    }
}
```
