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
$ faial-drf saxpy.cu
Kernel 'saxpy' is DRF!
```


# Checking a racy CUDA kernel

Let us now check a buggy protocol `saxpy-buggy.cu`. The difference between
`saxpy.cu` and `saxpy-buggy.cu` is simply changing the last read from `y[i]`
into `y[i + 1]`.


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
$  faial-drf saxpy-buggy.cu 
Kernel 'saxpy' has 1 data-race.

~~~~ Data-race 1 ~~~~

5 |   if (i < n) y[i] = a*x[i] + y[i + 1];
                 ────            ────────      
Globals
┌────────┬────────┐
│index   │1       │
├────────┼────────┤
│blockDim│x = 1024│
├────────┼────────┤
│n       │2       │
└────────┴────────┘

Locals
┌─────────┬─────┬─────┐
│threadIdx│x = 1│x = 0│
└─────────┴─────┴─────┘
(proof #0)
```

The error report consists of:
 * the source location (line 5) of the error, along with both access being highlighted (here underline)
 * In the table labelled `Global`, we have the state of thread-global program variables. An important characteristic of thread-global variables, is that threads observe the same value. Variable index represents the array index being accessed. Next, we have the value of variables `blockDim.x` and `n`. 
 * Next, we have the runtime state of thread-local variables (table `Locals`). There is one
   column per thread causing the data-race. In this case one thread
   `threadIdx.x=0` races with tread `threadIdx.x=1`.


