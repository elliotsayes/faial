// saxpy,ind,ctrl
// data flows from array to conditional
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (y[i]) x[i + 1] = a*x[i];
}
