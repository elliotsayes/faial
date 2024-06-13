__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  // blockDim.x iterations
  for (int i = 0; i <= threadIdx.x; i++) {
      y[i * threadIdx.x] = a*x[i];
  }
}
