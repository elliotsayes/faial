//saxpy,ind,ind
//_unknown_1
/*
Example 8: index loaded from an array and used in a loop, but loop
has no accesses.

The only approximate variable is `x[i]`:

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  for (int k = j; k < n; k++) {
    int w = k + 1;
  }
  x[i] = 1.0f * i;
}
