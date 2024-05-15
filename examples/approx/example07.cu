//saxpy,ind,ind
//
/*
Example 7: uses an index from a for loop.

There are no approximate variables.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  for (int j = 0; j < n; j++) {
    y[j] = i;
  }
}
