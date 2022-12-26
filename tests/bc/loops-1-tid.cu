__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  for (int i = threadIdx.x; i < n; i++) {
      y[2 * threadIdx.x] = a*x[i];
  }
}
