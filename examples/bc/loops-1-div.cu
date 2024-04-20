__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  for (int i = 512; i >= 1; i /= 2) {
    y[2 * threadIdx.x] = a*x[i];
  }
}
