__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  for (int i = 0; i < n; i++) {
    for (int j = 1; j < n; j *= 2) {
      y[2 * threadIdx.x] = a*x[i];
    }
  }
}
