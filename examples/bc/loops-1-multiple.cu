__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  for (int i = 0; i < 1; i += 1) {
    y[2 * threadIdx.x] = a*x[i];
    y[4 * threadIdx.x] = a*x[i];
    y[8 * threadIdx.x] = a*x[i];
  }
}
