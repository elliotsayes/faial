__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  // blockDim.x iterations
  for (int i = threadIdx.x; i <= 9; i++) {
      y[i * threadIdx.x] = a*x[i];
  }
}
