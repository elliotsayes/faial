Preamble:

  $ export EXE=../index_dep/main.exe

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

Example: data flows from array to conditional

  $ cat > example.cu << EOF
  > __global__ void saxpy(int n, float a, float *x, float *y) {
  >  int i = blockIdx.x*blockDim.x + threadIdx.x;
  >  if (y[i]) x[i + 1] = a*x[i];
  > }
  > EOF
  $ $EXE example.cu
  saxpy,ind,ctrl

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

Example: index loaded from an array and used in a loop, but loop
has no accesses.

  $ cat > example.cu << EOF
  > __global__ void saxpy(int n, float a, float *x, float *y) {
  >   int i = blockIdx.x*blockDim.x + threadIdx.x;
  >   int j = x[i];
  >   for (int k = j; k < n; k++) {
  >     int w = k + 1;
  >   }
  > }
  > EOF
  $ $EXE example.cu
  saxpy,ind,ind

Example: index loaded from array and used in a conditional, but conditional has
no accesses.

  $ cat > example.cu << EOF
  > __global__ void saxpy(int n, float a, float *x, float *y) {
  >   int i = blockIdx.x*blockDim.x + threadIdx.x;
  >   if (x[i]) {
  >     int w = i + 1;
  >   }
  > }
  > EOF
  $ $EXE example.cu
  saxpy,ind,ind
