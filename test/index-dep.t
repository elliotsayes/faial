Preamble:

  $ export EXE=../index_dep/main.exe
  $ approx() { ../drf/bin/main.exe --show-flat-acc "$1" | grep "approx locals" | cut -d":" -f2 | sed 's/ //'; }

# Input affects control flow

Example: integer flows from array index

  $ cat > example.cu << EOF
  > __global__ void saxpy(int n, float a, float *x, float *y) {
  >   int i = blockIdx.x*blockDim.x + threadIdx.x;
  >   int j = x[i];
  >   if (i < n) y[j] = a*j;
  > }
  > EOF
  $ $EXE example.cu
  saxpy,data,ind

Here variable `j` is renamed to `_unknown_1`:
  $ approx example.cu
  _unknown_1;

Example: data flows from array to conditional

  $ cat > example.cu << EOF
  > __global__ void saxpy(int n, float a, float *x, float *y) {
  >  int i = blockIdx.x*blockDim.x + threadIdx.x;
  >  if (y[i]) x[i + 1] = a*x[i];
  > }
  > EOF
  $ $EXE example.cu
  saxpy,ind,ctrl

Variable y[i] is _unknown_1 and x[i] is _unknown_2 both approximate.

  $ approx example.cu
  _unknown_1, _unknown_2;

Example: data flows from array to lower bound of loop

  $ cat > example.cu << EOF
  > __global__ void saxpy(int n, float a, float *x, float *y)
  > {
  >   int i = blockIdx.x*blockDim.x + threadIdx.x;
  >   int j = x[i]; // lower bound comes from array x
  >   for (int k = j; k < n; k++) {
  >     y[i] = y[i + 1];
  >   }
  > }
  > EOF
  $ $EXE example.cu
  saxpy,ind,ctrl

Variable `x[i]` is `_unkown_1`, `y[i + 1]` is `_unknown_2`.

  $ approx example.cu
  _unknown_1, _unknown_2, k;

Example: data flows from array to upper bound of loop

  $ cat > example.cu << EOF
  > __global__ void saxpy(int n, float a, float *x, float *y) {
  >   int i = blockIdx.x*blockDim.x + threadIdx.x;
  >   int j = x[i]; // upper bound comes from array x
  >   for (int k = 1; k < j; k++) {
  >     y[i + 1] = a * y[i];
  >   }
  > }
  > EOF
  $ $EXE example.cu
  saxpy,ind,ctrl

Approximate variables are `x[i]`, then `y[i]` and `k` (because
it depends on `x[i]`.

  $ approx example.cu
  _unknown_1, _unknown_2, k;

# Input affects array index

Index `j` is read from an array and then used to access array `y`.

  $ cat > example.cu << EOF
  > __global__ void saxpy(int n, float a, float *x, float *y) {
  >   int i = blockIdx.x*blockDim.x + threadIdx.x;
  >   int j = x[i];
  >   y[j] = a*j;
  > }
  > EOF
  $ $EXE example.cu
  saxpy,data,ind

The only approximate variable is `x[i]`:

  $ approx example.cu
  _unknown_1;


# Input affects array index and control-flow

Example: variables `j` and `k` come from an array and are used in a conditional.
Additionally, `k` is also used to index array `y`.

  $ cat > example.cu << EOF
  > __global__ void saxpy(int n, float a, float *x, float *y) {
  >   int i = blockIdx.x*blockDim.x + threadIdx.x;
  >   int j = x[i];
  >   int k = x[i + 1];
  >   float sum = 0.0f;
  >   if (j < k) {
  >     y[k] = sum;
  >   }
  > }
  > EOF
  $ $EXE example.cu
  saxpy,data,ctrl

Approximate variables are `x[i]` and `x[i + 1]`:

  $ approx example.cu
  _unknown_1, _unknown_2;

# Input does not affect


Example: uses an index from a for loop.

  $ cat > example.cu << EOF
  > __global__ void saxpy(int n, float a, float *x, float *y) {
  >   int i = blockIdx.x*blockDim.x + threadIdx.x;
  >   for (int j = 0; j < n; j++) {
  >     y[j] = i;
  >   }
  > }
  > EOF
  $ $EXE example.cu
  saxpy,ind,ind

There are no approximate variables:

  $ approx example.cu
  ;

Example: index loaded from an array and used in a loop, but loop
has no accesses.

  $ cat > example.cu << EOF
  > __global__ void saxpy(int n, float a, float *x, float *y) {
  >   int i = blockIdx.x*blockDim.x + threadIdx.x;
  >   int j = x[i];
  >   for (int k = j; k < n; k++) {
  >     int w = k + 1;
  >   }
  >   x[i] = 1.0f * i;
  > }
  > EOF
  $ $EXE example.cu
  saxpy,ind,ind

The only approximate variable is `x[i]`:

  $ approx example.cu
  _unknown_1;

Example: index loaded from array and used in a conditional, but conditional has
no accesses.

  $ cat > example.cu << EOF
  > __global__ void saxpy(int n, float a, float *x, float *y) {
  >   int i = blockIdx.x*blockDim.x + threadIdx.x;
  >   if (x[i]) {
  >     int z = i + 1;
  >   }
  >   x[i] = 1.0f * i;
  > }
  > EOF
  $ $EXE example.cu
  saxpy,ind,ind

The only approximate variable is `x[i]`:

  $ approx example.cu
  _unknown_1;

# Exact and approximate variables

Second loop variable is rendered as j1 and x[i] as unknonw_1.

  $ cat > example.cu << EOF
  > __global__ void saxpy(int n, float a, float *x, float *y) {
  >   int i = blockIdx.x*blockDim.x + threadIdx.x;
  >   for (int j = 0; j < n; j++) {
  >     y[j] = i;
  >   }
  >   for (int j = x[i]; j < n; j++) {
  >     y[j] = i;
  >   }
  > }
  > EOF
  $ approx example.cu
  _unknown_1, j1;

