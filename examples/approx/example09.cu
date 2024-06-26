//saxpy,ind,ind
//
/*
Example 9: index loaded from array and used in a conditional, but conditional
has no accesses.

The only approximate variable is `x[i]`, which is not used to access any
array.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (x[i]) {
    int z = i + 1;
  }
  x[i] = 1.0f * i;
}
