__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  for (int i = 1; i < n; i *= 2) {
    y[32 * threadIdx.x] = a*x[i];
  }
}
