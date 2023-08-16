// saxpy,ind,ind
// Array data does not affect accesses.
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  for (int j = 0; j < n; j++) {
    if (i) {
      y[i] = x[i];
    }
  }
}
