__global__
void saxpy(unsigned int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  float f;
  y[0] = 0;
  for (int j = 1; j <= n; j++) {
    f += y[j];
  }
  y[i + n + 1] = a*x[i];
}
