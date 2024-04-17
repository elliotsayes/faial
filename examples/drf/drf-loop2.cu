__global__
void saxpy(unsigned int n, float a, float *x, float *y)
{
  float f = 0.0f;
  __assume(n > 0);
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  for (int j = n; j >= 0; j--) {
    f += y[j];
  }
  int offset = 1;
  y[i + n + 1] = a*x[i] + f;
}
