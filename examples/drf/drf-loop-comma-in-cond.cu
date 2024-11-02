__global__
void saxpy(int n, float a, float *x, float *y)
{
  __requires (n > 0);
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  float f = 0.0f;
  y[0] = 0;
  for (int j = 1; x[0], j < n; j++) {
    f += y[j];
    // y[0] ... y[n - 1]
  }
  // y[ 0 + n]
  y[i + n] = f;
}
