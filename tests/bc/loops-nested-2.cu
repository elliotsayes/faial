__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  for (int i = 0; i < n; i++) {
    for (int j = i; j < n; j++) {
      y[2 * threadIdx.x] = a*x[i];
    }
  }

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      y[2 * threadIdx.x] = a*x[i];
    }
  }
}
