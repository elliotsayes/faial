// saxpy,ind,ctrl
// data flows from array to upper bound of loop
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  for (int k = 1; k < j; k++) {
    y[i + 1] = a * y[i];
  }
}
