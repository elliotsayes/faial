__global__
void saxpy(int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  float f;
  for (int j = 0; j <= n; j++) {
    f += y[j];
  }
  y[i + n + 1] = a*x[i];
}
