// saxpy,ind,ind
// Input does not affect access
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  for (int j = 0; j < n; j++) {
    y[j] = i;
  }
}
