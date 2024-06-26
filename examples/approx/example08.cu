//saxpy,ind,ind
//
/*
Example 8: index loaded from an array and used in a loop, but loop
has no accesses.

- `j` is approximate because it is loaded from `x[i]`
- `k` is approximate because it depends on `j`
- `w` is approximate because it depends on `k`

The loop has no accesses, so it is elided, hence no approximate variables.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  for (int k = j; k < n; k++) {
    int w = k + 1;
  }
  x[i] = 1.0f * i;
}
