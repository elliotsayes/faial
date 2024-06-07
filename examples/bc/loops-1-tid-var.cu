__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  // blockDim.x iterations
  for (int i = 0; i <= threadIdx.x; i++) {
      y[2 * threadIdx.x] = a*x[i];
  }
  // blockDim.x iterations
  for (int i = threadIdx.x; i <= blockDim.x - 1; i++) {
      y[2 * threadIdx.x] = a*x[i];
  }
}
